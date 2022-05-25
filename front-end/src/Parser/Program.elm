module Parser.Program exposing
    ( BinaryExpression(..)
    , CallExpr(..)
    , Expr(..)
    , Function(..)
    , LampProgramTree(..)
    , Statement(..)
    , Type(..)
    , UnaryExpression(..)
    , Variable(..)
    , programParser
    )

import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extra exposing (..)
import Parser.Extras exposing (..)
import Set


type Function
    = Function String (List Variable) Type (List Variable) (List Statement)


type Variable
    = Variable String Type


type LampProgramTree
    = LampProgram (List Variable) (List Function)


type Type
    = Integer
    | Float
    | IntegerArray
    | FloatArray


type Statement
    = CallStatement CallExpr
    | IfStatement Expr (List Statement)
    | IfElseStatement Expr (List Statement) (List Statement)
    | WhileStatement Expr (List Statement)
    | Assignment String Expr
    | ArrayAssignment String Expr Expr
    | CreateArrayAssignment String Type Expr
    | EchoStatement Expr
    | ReturnStatement Expr


type CallExpr
    = CallByName String (List Expr)


type Expr
    = IfExpr
    | CallExpr CallExpr
    | VariableExpr String
    | IntExpr Int
    | FloatExpr Float
    | IndexExpr Expr Expr
    | BinaryExpression BinaryExpression
    | UnaryExpression UnaryExpression


type BinaryExpression
    = AndExpr Expr Expr
    | OrExpr Expr Expr
    | LessThanExpr Expr Expr
    | LessThanEqualsExpr Expr Expr
    | GreaterThanEqualsExpr Expr Expr
    | GreaterThanExpr Expr Expr
    | EqualsExpr Expr Expr
    | NotEqualsExpr Expr Expr
    | AddExpr Expr Expr
    | SubtractExpr Expr Expr
    | MultiplyExpr Expr Expr
    | DivideExpr Expr Expr
    | ModuloExpr Expr Expr


type UnaryExpression
    = NegativeExpr Expr
    | NotExpr Expr


identifier : Parser String
identifier =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "while", "if", "return" ]
        }
        |. spaces


typea : Parser Type
typea =
    oneOf
        [ succeed Integer |. keyword "Int"
        , succeed Float |. keyword "Float"
        , succeed IntegerArray |. keyword "IntegerArray"
        , succeed FloatArray |. keyword "FloatArray"
        ]


arrayType : Parser Type
arrayType =
    oneOf
        [ succeed IntegerArray |. keyword "IntegerArray"
        , succeed FloatArray |. keyword "FloatArray"
        ]
        |. spaces


variable : Parser Variable
variable =
    succeed Variable
        |= identifier
        |. symbolSpaces ":"
        |= typea


variableDeclaration : Parser Variable
variableDeclaration =
    succeed identity
        |. keyword "var"
        |. spaces
        |= variable


declarations : Parser (List Variable)
declarations =
    many variableDeclaration


parameterList : Parser (List Variable)
parameterList =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = spaces
        , item = variable
        , trailing = Forbidden
        }


expression : Parser Expr
expression =
    buildExpressionParser operators (lazy <| \_ -> term)


assignmentOrCall : Parser Statement
assignmentOrCall =
    succeed (\id fn -> fn id)
        |= identifier
        |= oneOf
            [ succeed (\b c a -> ArrayAssignment a b c)
                |. symbolSpaces "["
                |= expression
                |. spaces
                |. symbolSpaces "]"
                |. symbolSpaces "="
                |= expression
            , succeed (\es id -> CallStatement (CallByName id es))
                |= exprList
            , succeed identity
                |. symbolSpaces "="
                |= oneOf
                    [ succeed (flip Assignment)
                        |= expression
                    , succeed (\b c a -> CreateArrayAssignment a b c)
                        |= arrayType
                        |. symbolSpaces "["
                        |= expression
                        |. spaces
                        |. symbolSpaces "]"
                    ]
            ]
        |. symbol ";"


statementBlock : Parser (List Statement)
statementBlock =
    succeed identity
        |. symbolSpaces "{"
        |= (lazy <| \_ -> statements)
        |. spaces
        |. symbolSpaces "}"


ifElseStatement : Parser Statement
ifElseStatement =
    succeed (\c t fn -> fn c t)
        |. keyword "if"
        |. spaces
        |. symbolSpaces "("
        |= expression
        |. spaces
        |. symbolSpaces ")"
        |= statementBlock
        |= oneOf
            [ succeed (\f c t -> IfElseStatement c t f)
                |. keyword "else"
                |. spaces
                |= statementBlock
            , succeed IfStatement
            ]


whileStatement : Parser Statement
whileStatement =
    succeed WhileStatement
        |. keyword "while"
        |. spaces
        |. symbolSpaces "("
        |= expression
        |. spaces
        |. symbolSpaces ")"
        |= statementBlock


returnStatement : Parser Statement
returnStatement =
    succeed ReturnStatement
        |. keyword "return"
        |. spaces
        |= expression
        |. spaces
        |. symbolSpaces ";"


statement : Parser Statement
statement =
    oneOf
        [ assignmentOrCall
        , ifElseStatement
        , whileStatement
        , returnStatement
        ]


statements : Parser (List Statement)
statements =
    many statement


function : Parser Function
function =
    succeed Function
        |. keyword "fun"
        |. spaces
        |= identifier
        |= parameterList
        |. spaces
        |. symbolSpaces ":"
        |= typea
        |. spaces
        |. symbolSpaces "{"
        |= declarations
        |. spaces
        |= statements
        |. spaces
        |. symbolSpaces "}"


functions : Parser (List Function)
functions =
    some function |> map (\( v, vl ) -> v :: vl)


programParser : Parser LampProgramTree
programParser =
    succeed LampProgram
        |= declarations
        |= functions
        |. end


binExpr : (Expr -> Expr -> BinaryExpression) -> Expr -> Expr -> Expr
binExpr f l r =
    f l r |> BinaryExpression


operators : OperatorTable Expr
operators =
    [ [ prefixOperator (NegativeExpr >> UnaryExpression) (symbol "-")
      , prefixOperator (NotExpr >> UnaryExpression) (symbol "!")
      , prefixOperator identity (symbol "+")
      ]
    , [ infixOperator (binExpr MultiplyExpr) (symbol "*") AssocLeft
      , infixOperator (binExpr DivideExpr) (symbol "/") AssocLeft
      , infixOperator (binExpr ModuloExpr) (symbol "%") AssocLeft
      ]
    , [ infixOperator (binExpr AddExpr) (symbol "+") AssocLeft
      , infixOperator (binExpr SubtractExpr) (symbol "-") AssocLeft
      ]
    , [ infixOperator (binExpr LessThanExpr) (symbol "<") AssocLeft
      , infixOperator (binExpr LessThanEqualsExpr) (symbol "<=") AssocLeft
      , infixOperator (binExpr GreaterThanEqualsExpr) (symbol ">=") AssocLeft
      , infixOperator (binExpr GreaterThanExpr) (symbol ">") AssocLeft
      , infixOperator (binExpr EqualsExpr) (symbol "==") AssocLeft
      , infixOperator (binExpr NotEqualsExpr) (symbol "!=") AssocLeft
      ]
    , [ infixOperator (binExpr AndExpr) (symbol "&&") AssocLeft
      ]
    , [ infixOperator (binExpr OrExpr) (symbol "||") AssocLeft
      ]
    ]


primary : Parser Expr
primary =
    number
        { int = Just IntExpr
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just FloatExpr
        }
        |. spaces


exprList : Parser (List Expr)
exprList =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = spaces
        , item = lazy <| \_ -> expression
        , trailing = Forbidden -- demand a trailing semi-colon
        }


identifierMember : Parser Expr
identifierMember =
    succeed (\id fn -> fn id)
        |= identifier
        |= oneOf
            [ succeed (\es id -> CallExpr (CallByName id es))
                |= exprList
            , succeed VariableExpr
            ]


member : Parser Expr
member =
    oneOf
        [ identifierMember
        , succeed identity
            |. symbolSpaces "("
            |= (lazy <| \_ -> expression)
            |. symbolSpaces ")"
        ]


memberTerm : Parser Expr
memberTerm =
    succeed (\m fn -> fn m)
        |= member
        |= oneOf
            [ succeed (flip IndexExpr)
                |. symbolSpaces "["
                |= expression
                |. symbolSpaces "]"
            , succeed identity
            ]


term : Parser Expr
term =
    oneOf
        [ memberTerm
        , primary
        ]


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b


symbolSpaces : String -> Parser ()
symbolSpaces s =
    symbol s
        |. spaces
