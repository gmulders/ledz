module Parser.Assemble exposing (..)

import Dict exposing (Dict, empty, get, union)
import Html exposing (b, param)
import List exposing (append, filter, foldl, foldr, head, indexedMap, length, partition, reverse)
import Maybe exposing (withDefault)
import Parser.Opcode exposing (..)
import Parser.Program
    exposing
        ( BinaryExpression(..)
        , CallExpr(..)
        , Expr(..)
        , Function(..)
        , LampProgramTree(..)
        , Statement(..)
        , Type(..)
        , UnaryExpression(..)
        , Variable(..)
        )


type SemanticError
    = UnknownFunction String
    | UnknownVariable String
    | NoMainFunctionFound
    | ImpossibleCast Type Type
    | WrongArgumentCount (List Variable) (List Expr)
    | NotAnArray Expr


type alias Assembly =
    Result (List SemanticError) (List OpCode)


type alias GlobalContext =
    { functions : Dict String PoolFunction
    , variables : Dict String PoolVariable
    }


type alias Context =
    { functions : Dict String PoolFunction
    , variables : Dict String PoolVariable
    , function : PoolFunction
    }


type alias PoolFunction =
    { key : Int
    , name : String
    , parameters : List Variable
    , returnType : Type
    , locals : List Variable
    , statements : List Statement
    , isInternal : Bool
    }


type alias PoolVariable =
    { place : VariablePlace
    , varTypeType : VariableType
    , name : String
    , varType : Type
    , key : Int
    }


type VariableType
    = Array
    | Primitive


type VariablePlace
    = Global
    | Local
    | Parameter


assemble : LampProgramTree -> List PoolFunction -> Assembly
assemble (LampProgram globals fns) internalFns =
    let
        programFns =
            indexedMap toPoolFunction fns

        fnDict =
            append programFns internalFns
                |> List.map (\pfn -> ( pfn.name, pfn ))
                |> Dict.fromList

        ctx =
            { functions = fnDict
            , variables = buildVariableDict globals Global
            }
    in
    concat
        [ initCall ctx
        , mainCall ctx
        , assembleFunctions ctx programFns
        ]
        |> linkAssembly


linkAssembly : Assembly -> Assembly
linkAssembly asly =
    case asly of
        Ok code ->
            linkCode code

        Err _ ->
            asly


linkCode : List OpCode -> Assembly
linkCode code =
    let
        functionOffsets =
            calculateFunctionOffsets code
                |> Dict.fromList
    in
    Ok (mapWithPrevious (completeCallOpCode functionOffsets) code)


initCall : GlobalContext -> Assembly
initCall ctx =
    case get "init" ctx.functions of
        Just f ->
            concat
                [ Ok
                    [ DupX1
                    , BranchTrue 8
                    , Dup
                    ]
                , assembleFunctionCall f
                , Ok [ Pop, Swap ]
                ]

        _ ->
            emptyAssembly


mainCall : GlobalContext -> Assembly
mainCall ctx =
    case get "main" ctx.functions of
        Just f ->
            assembleFunctionCall f

        _ ->
            Err [ NoMainFunctionFound ]


completeCallOpCode : Dict String Int -> Maybe OpCode -> OpCode -> OpCode
completeCallOpCode functionOffsets previous current =
    case previous of
        Just (Annotation (FunctionCall s)) ->
            case current of
                Call _ b c d e ->
                    Call (get s functionOffsets |> withDefault -1) b c d e

                _ ->
                    current

        _ ->
            current


calculateFunctionOffsets : List OpCode -> List ( String, Int )
calculateFunctionOffsets list =
    calculateFunctionOffsetsHelper (reverse list)


calculateFunctionOffsetsHelper : List OpCode -> List ( String, Int )
calculateFunctionOffsetsHelper list =
    case list of
        [] ->
            []

        (Annotation (FunctionDef s)) :: xs ->
            ( s, assemblySize xs ) :: calculateFunctionOffsetsHelper xs

        s :: xs ->
            calculateFunctionOffsetsHelper xs


toPoolFunction : Int -> Function -> PoolFunction
toPoolFunction index (Function name parameters returnType locals statements) =
    { key = index
    , name = name
    , parameters = parameters
    , returnType = returnType
    , locals = locals
    , statements = statements
    , isInternal = False
    }


assembleFunctions : GlobalContext -> List PoolFunction -> Assembly
assembleFunctions ctx fns =
    fns
        |> List.map (assembleFunction ctx)
        |> concat


assembleFunction : GlobalContext -> PoolFunction -> Assembly
assembleFunction ctx fn =
    -- TODO: Check for collisions in locals and parameters
    let
        -- (Function name parameters _ locals statements) =
        --     fn
        variables =
            [ buildVariableDict (reverse fn.parameters) Parameter, buildVariableDict fn.locals Local, ctx.variables ]
                |> List.foldl union empty

        fnCtx =
            { functions = ctx.functions
            , function = fn
            , variables = variables
            }

        assembly =
            fn.statements
                |> List.map (assembleStatement fnCtx)
                |> concat
    in
    concat
        [ opToAsly (Annotation (FunctionDef fn.name))
        , assembly
        ]


buildVariableDict : List Variable -> VariablePlace -> Dict String PoolVariable
buildVariableDict vars place =
    let
        ( primitives, arrays ) =
            vars
                |> partition isPrimitiveVariable
                |> Tuple.mapBoth (buildPoolVariables place Primitive) (buildPoolVariables place Array)
    in
    [ primitives, arrays ]
        |> foldr (++) []
        |> List.map toNameTuple
        |> Dict.fromList


toNameTuple : PoolVariable -> ( String, PoolVariable )
toNameTuple pv =
    ( pv.name, pv )


isPrimitiveVariable : Variable -> Bool
isPrimitiveVariable (Variable _ t) =
    isPrimitive t


buildPoolVariables : VariablePlace -> VariableType -> List Variable -> List PoolVariable
buildPoolVariables place varType variables =
    let
        keyFn =
            keyFns place varType
    in
    variables
        |> indexedMap (\index (Variable name t) -> PoolVariable place varType name t (keyFn index))


keyFns : VariablePlace -> VariableType -> (Int -> Int)
keyFns p t =
    case ( p, t ) of
        ( Local, Primitive ) ->
            \i -> i + 1

        ( Local, Array ) ->
            identity

        ( Parameter, Primitive ) ->
            \i -> -(i + 5)

        ( Parameter, Array ) ->
            \i -> -(i + 1)

        ( Global, Primitive ) ->
            identity

        ( Global, Array ) ->
            identity


variableType : Variable -> Type
variableType (Variable _ varType) =
    varType


isPrimitive : Type -> Bool
isPrimitive t =
    t == Integer || t == Float


assembleStatements : Context -> List Statement -> Assembly
assembleStatements ctx statements =
    List.map (assembleStatement ctx) statements
        |> concat


assembleStatement : Context -> Statement -> Assembly
assembleStatement ctx stmt =
    case stmt of
        CallStatement callExpr ->
            concat
                [ assembleCallExpression ctx callExpr
                , opToAsly Pop
                ]

        IfStatement condition statements ->
            assembleIfStatement ctx condition statements

        IfElseStatement condition trueStatements falseStatements ->
            assembleIfElseStatement ctx condition trueStatements falseStatements

        WhileStatement condition statements ->
            assembleWhileStatement ctx condition statements

        Assignment name expr ->
            assembleAssignment ctx name expr

        ArrayAssignment name indexExpr expr ->
            assembleArrayAssignment ctx name indexExpr expr

        CreateArrayAssignment name arrayType sizeExpr ->
            assembleCreateArrayAssignment ctx name arrayType sizeExpr

        EchoStatement expr ->
            emptyAssembly

        ReturnStatement expr ->
            assembleReturnStatement ctx expr


assembleIfStatement : Context -> Expr -> List Statement -> Assembly
assembleIfStatement ctx condition statements =
    -- TODO: Check if condition is an expression
    let
        conditionAssembly =
            assembleExpression ctx condition

        trueStatementsAssembly =
            assembleStatements ctx statements
    in
    case trueStatementsAssembly of
        Ok tc ->
            concat
                [ conditionAssembly
                , opToAsly (BranchFalse (assemblySize tc))
                , trueStatementsAssembly
                ]

        _ ->
            concat
                [ conditionAssembly
                , trueStatementsAssembly
                ]


assembleIfElseStatement : Context -> Expr -> List Statement -> List Statement -> Assembly
assembleIfElseStatement ctx condition trueStatements falseStatements =
    -- TODO: Check if condition is an expression
    let
        conditionAssembly =
            assembleExpression ctx condition

        trueStatementsAssembly =
            assembleStatements ctx trueStatements

        falseStatementsAssembly =
            assembleStatements ctx falseStatements
    in
    case ( trueStatementsAssembly, falseStatementsAssembly ) of
        ( Ok tc, Ok fc ) ->
            concat
                [ conditionAssembly
                , opToAsly (BranchFalse (assemblySize tc + branchSize))
                , trueStatementsAssembly
                , opToAsly (Branch (assemblySize fc))
                , falseStatementsAssembly
                ]

        _ ->
            concat
                [ conditionAssembly
                , trueStatementsAssembly
                , falseStatementsAssembly
                ]


assembleWhileStatement : Context -> Expr -> List Statement -> Assembly
assembleWhileStatement ctx condition statements =
    let
        conditionAssembly =
            assembleExpression ctx condition

        statementsAssembly =
            assembleStatements ctx statements
    in
    case ( conditionAssembly, statementsAssembly ) of
        ( Ok cc, Ok wc ) ->
            concat
                [ conditionAssembly
                , opToAsly (BranchFalse (assemblySize wc + branchSize))
                , statementsAssembly
                , opToAsly (Branch -(assemblySize cc + branchSize + assemblySize wc + branchSize))
                ]

        _ ->
            concat
                [ conditionAssembly
                , statementsAssembly
                ]


assembleAssignment : Context -> String -> Expr -> Assembly
assembleAssignment ctx name expr =
    -- TODO: Check that the variable type is an array type
    let
        variable =
            fetchVariable ctx name

        newValueAsly =
            variable
                |> Result.map .varType
                |> Result.andThen (castAssembly ctx expr)

        storeAsly =
            variable
                |> Result.andThen storeVariableAssembly
    in
    concat
        [ newValueAsly
        , storeAsly
        ]


assembleArrayAssignment : Context -> String -> Expr -> Expr -> Assembly
assembleArrayAssignment ctx name indexExpr expr =
    -- TODO: Check that the variable type is an array type
    let
        variable =
            fetchVariable ctx name

        loadVariableAsly =
            variable
                |> Result.andThen loadVariableAssembly

        newValueAsly =
            variable
                |> Result.map (.varType >> baseType)
                |> Result.andThen (castAssembly ctx expr)
    in
    concat
        [ loadVariableAsly
        , newValueAsly
        , castAssembly ctx indexExpr Integer
        , opToAsly ArrayStore
        ]


baseType : Type -> Type
baseType t =
    case t of
        IntegerArray ->
            Integer

        FloatArray ->
            Float

        _ ->
            Integer


assembleCreateArrayAssignment : Context -> String -> Type -> Expr -> Assembly
assembleCreateArrayAssignment ctx name arrayType sizeExpr =
    let
        asly =
            fetchVariable ctx name
                |> Result.andThen storeVariableAssembly
    in
    concat
        [ castAssembly ctx sizeExpr Integer
        , opToAsly NewArray
        , asly
        ]


assembleReturnStatement : Context -> Expr -> Assembly
assembleReturnStatement ctx expr =
    let
        v =
            if isPrimitive ctx.function.returnType then
                1

            else
                2
    in
    concat
        [ castAssembly ctx expr ctx.function.returnType
        , opToAsly (Return v)
        ]


storeVariableAssembly : PoolVariable -> Assembly
storeVariableAssembly pv =
    opToAsly (storeOpCode pv pv.key)


storeOpCode : PoolVariable -> (Int -> OpCode)
storeOpCode pv =
    case ( pv.place, pv.varTypeType ) of
        ( Global, Primitive ) ->
            GlobalStore

        ( Global, Array ) ->
            GlobalStoreObject

        ( _, Primitive ) ->
            Store

        ( _, Array ) ->
            StoreObject


loadVariableAssembly : PoolVariable -> Assembly
loadVariableAssembly pv =
    opToAsly (loadOpCode pv pv.key)


loadOpCode : PoolVariable -> (Int -> OpCode)
loadOpCode pv =
    case ( pv.place, pv.varTypeType ) of
        ( Global, Primitive ) ->
            GlobalLoad

        ( Global, Array ) ->
            GlobalLoadObject

        ( _, Primitive ) ->
            Load

        ( _, Array ) ->
            LoadObject


assembleCallExpression : Context -> CallExpr -> Assembly
assembleCallExpression ctx (CallByName name exprs) =
    let
        poolFn =
            get name ctx.functions
                |> Result.fromMaybe [ UnknownFunction name ]

        paramsAssembly =
            poolFn
                |> Result.map .parameters
                |> Result.andThen (checkParams exprs)
                |> Result.map (List.map (\(Variable _ typ) -> typ))
                |> Result.map (zip exprs)
                |> Result.map (List.map (\( e, t ) -> castAssembly ctx e t))
                |> Result.andThen concat

        assembleForPoolFn =
            \pFn ->
                if pFn.isInternal then
                    assembleInternalCall ctx pFn exprs

                else
                    assembleFunctionCall pFn

        callAssembly =
            poolFn
                |> Result.andThen assembleForPoolFn
    in
    concat [ paramsAssembly, callAssembly ]


checkParams : List Expr -> List Variable -> Result (List SemanticError) (List Variable)
checkParams exprs variables =
    if List.length exprs /= List.length variables then
        Err [ WrongArgumentCount variables exprs ]

    else
        Ok variables


zip : List a -> List b -> List ( a, b )
zip la lb =
    List.map2 Tuple.pair la lb


assembleInternalCall : Context -> PoolFunction -> List Expr -> Assembly
assembleInternalCall ctx poolFn exprs =
    concat
        [ opToAsly (Annotation (FunctionCall poolFn.name))
        , opToAsly (CallIn poolFn.key)
        ]


assembleFunctionCall : PoolFunction -> Assembly
assembleFunctionCall poolFn =
    let
        argPrimCount =
            List.map variableType poolFn.parameters
                |> filter isPrimitive
                |> length

        localPrimCount =
            List.map variableType poolFn.locals
                |> filter isPrimitive
                |> length

        argObjCount =
            List.map variableType poolFn.parameters
                |> filter (isPrimitive >> not)
                |> length

        localObjCount =
            List.map variableType poolFn.locals
                |> filter (isPrimitive >> not)
                |> length
    in
    concat
        [ opToAsly (Annotation (FunctionCall poolFn.name))
        , opToAsly (Call 0 argPrimCount localPrimCount argObjCount localObjCount)
        ]


assembleExpressions : Context -> List Expr -> Assembly
assembleExpressions ctx exprs =
    exprs
        |> List.map (assembleExpression ctx)
        |> concat


assembleExpression : Context -> Expr -> Assembly
assembleExpression ctx expr =
    case expr of
        IfExpr ->
            emptyAssembly

        -- ignore
        CallExpr call ->
            assembleCallExpression ctx call

        VariableExpr name ->
            assembleVariableExpr ctx name

        IntExpr value ->
            opToAsly (IntConst value)

        FloatExpr value ->
            opToAsly (FloatConst value)

        IndexExpr array index ->
            assembleIndexExpr ctx array index

        BinaryExpression (AndExpr l r) ->
            binExpr ctx l r (\_ -> BoolAnd)

        BinaryExpression (OrExpr l r) ->
            binExpr ctx l r (\_ -> BoolOr)

        BinaryExpression (LessThanExpr l r) ->
            binExpr ctx l r (switchOpCode FloatLt IntLt)

        BinaryExpression (LessThanEqualsExpr l r) ->
            binExpr ctx l r (switchOpCode FloatLte IntLte)

        BinaryExpression (GreaterThanEqualsExpr l r) ->
            binExpr ctx l r (switchOpCode FloatGte IntGte)

        BinaryExpression (GreaterThanExpr l r) ->
            binExpr ctx l r (switchOpCode FloatGt IntGt)

        BinaryExpression (EqualsExpr l r) ->
            binExpr ctx l r (switchOpCode FloatEq IntEq)

        BinaryExpression (NotEqualsExpr l r) ->
            concat
                [ binExpr ctx l r (switchOpCode FloatEq IntEq)
                , opToAsly BoolNot
                ]

        BinaryExpression (AddExpr l r) ->
            binExpr ctx l r (switchOpCode FloatAdd IntAdd)

        BinaryExpression (SubtractExpr l r) ->
            binExpr ctx l r (switchOpCode FloatSub IntSub)

        BinaryExpression (MultiplyExpr l r) ->
            binExpr ctx l r (switchOpCode FloatMul IntMul)

        BinaryExpression (DivideExpr l r) ->
            binExpr ctx l r (switchOpCode FloatDiv IntDiv)

        BinaryExpression (ModuloExpr l r) ->
            binExpr ctx l r (\_ -> IntMod)

        UnaryExpression (NegativeExpr a) ->
            unExpr ctx a (switchOpCode FloatNeg IntNeg)

        UnaryExpression (NotExpr a) ->
            unExpr ctx a (\_ -> BoolNot)


assembleVariableExpr : Context -> String -> Assembly
assembleVariableExpr ctx name =
    fetchVariable ctx name
        |> Result.andThen loadVariableAssembly


assembleIndexExpr : Context -> Expr -> Expr -> Assembly
assembleIndexExpr ctx arrayExpr index =
    let
        arrayAssembly =
            checkArrayExprType ctx arrayExpr
                |> Result.andThen (assembleExpression ctx)

        indexAssembly =
            castAssembly ctx index Integer
    in
    concat
        [ arrayAssembly
        , indexAssembly
        , opToAsly ArrayLoad
        ]


checkArrayExprType : Context -> Expr -> Result (List SemanticError) Expr
checkArrayExprType ctx arrayExpr =
    if isPrimitive (exprType ctx arrayExpr) then
        Err [ NotAnArray arrayExpr ]

    else
        Ok arrayExpr


switchOpCode : OpCode -> OpCode -> Type -> OpCode
switchOpCode floatOc intOc t =
    if t == Float then
        floatOc

    else
        intOc


binExpr : Context -> Expr -> Expr -> (Type -> OpCode) -> Assembly
binExpr ctx l r oc =
    let
        expectedType =
            binExprType ctx l r

        left =
            castAssembly ctx l expectedType

        right =
            castAssembly ctx r expectedType
    in
    concat [ left, right, opToAsly (oc expectedType) ]


unExpr : Context -> Expr -> (Type -> OpCode) -> Assembly
unExpr ctx arg oc =
    let
        valueType =
            exprType ctx arg

        assembly =
            assembleExpression ctx arg
    in
    concat [ assembly, opToAsly (oc valueType) ]


castAssembly : Context -> Expr -> Type -> Assembly
castAssembly ctx e toType =
    let
        castOperand =
            case toType of
                Integer ->
                    Float2Int

                _ ->
                    Int2Float

        assembly =
            assembleExpression ctx e

        fromType =
            exprType ctx e

        cast =
            if (not << isPrimitive) fromType then
                Err [ ImpossibleCast fromType toType ]

            else if fromType == toType then
                emptyAssembly

            else
                opToAsly castOperand
    in
    concat [ assembly, cast ]


emptyAssembly : Assembly
emptyAssembly =
    Ok []


opToAsly : OpCode -> Assembly
opToAsly oc =
    Ok [ oc ]


combineAssembly : Assembly -> Assembly -> Assembly
combineAssembly l r =
    case ( l, r ) of
        ( Ok lv, Ok rv ) ->
            Ok (append lv rv)

        ( Ok _, Err re ) ->
            Err re

        ( Err le, Ok _ ) ->
            Err le

        ( Err le, Err re ) ->
            Err (append le re)


concat : List Assembly -> Assembly
concat list =
    foldr combineAssembly emptyAssembly list


exprType : Context -> Expr -> Type
exprType ctx expr =
    case expr of
        IfExpr ->
            Integer

        -- TODO: Validate that the function exists
        CallExpr (CallByName id _) ->
            get id ctx.functions
                |> Maybe.map .returnType
                |> withDefault Integer

        -- TODO: Validate that the variable exists
        VariableExpr name ->
            get name ctx.variables
                |> Maybe.map (\pv -> pv.varType)
                |> withDefault Integer

        IntExpr _ ->
            Integer

        FloatExpr _ ->
            Float

        -- TODO: Validate that the type of the array is never an Integer nor a Float; create validator
        IndexExpr array _ ->
            case exprType ctx array of
                Integer ->
                    Integer

                Float ->
                    Float

                IntegerArray ->
                    Integer

                FloatArray ->
                    Float

        BinaryExpression (AndExpr _ _) ->
            Integer

        BinaryExpression (OrExpr _ _) ->
            Integer

        BinaryExpression (LessThanExpr _ _) ->
            Integer

        BinaryExpression (LessThanEqualsExpr _ _) ->
            Integer

        BinaryExpression (GreaterThanEqualsExpr _ _) ->
            Integer

        BinaryExpression (GreaterThanExpr _ _) ->
            Integer

        BinaryExpression (EqualsExpr _ _) ->
            Integer

        BinaryExpression (NotEqualsExpr _ _) ->
            Integer

        BinaryExpression (AddExpr l r) ->
            binExprType ctx l r

        BinaryExpression (SubtractExpr l r) ->
            binExprType ctx l r

        BinaryExpression (MultiplyExpr l r) ->
            binExprType ctx l r

        BinaryExpression (DivideExpr l r) ->
            binExprType ctx l r

        BinaryExpression (ModuloExpr _ _) ->
            -- TODO: Validate that the types are Integer
            Integer

        UnaryExpression (NegativeExpr a) ->
            -- TODO: Validate that the type is primitive
            exprType ctx a

        UnaryExpression (NotExpr _) ->
            -- TODO: Validate that the type is always Integer
            Integer


binExprType : Context -> Expr -> Expr -> Type
binExprType ctx l r =
    -- TODO: Validate that the types are primitive
    case ( exprType ctx l, exprType ctx r ) of
        ( Integer, Float ) ->
            Float

        ( Float, _ ) ->
            Float

        _ ->
            Integer


mapWithPrevious : (Maybe a -> a -> b) -> List a -> List b
mapWithPrevious fn list =
    case list of
        [] ->
            []

        x :: _ ->
            fn Nothing x :: mapWithPreviousHelper fn list


mapWithPreviousHelper : (Maybe a -> a -> b) -> List a -> List b
mapWithPreviousHelper fn list =
    case list of
        [] ->
            []

        _ :: [] ->
            []

        x1 :: x2 :: xs ->
            fn (Just x1) x2 :: mapWithPrevious fn (x2 :: xs)


fetchVariable : Context -> String -> Result (List SemanticError) PoolVariable
fetchVariable ctx name =
    get name ctx.variables
        |> Result.fromMaybe [ UnknownVariable name ]
