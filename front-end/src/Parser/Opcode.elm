module Parser.Opcode exposing (AnnotationType(..), OpCode(..), assemblySize, branchSize, opcodeSize, opcodeToEncoder)

import Bytes exposing (Endianness(..))
import Bytes.Encode exposing (Encoder, float32, sequence, signedInt32)
import List exposing (map)


type OpCode
    = NoOp
    | IntConst Int
    | IntAdd
    | IntSub
    | IntMul
    | IntDiv
    | IntMod
    | IntNeg
    | IntLt
    | IntLte
    | IntEq
    | IntGte
    | IntGt
    | FloatConst Float
    | FloatAdd
    | FloatSub
    | FloatMul
    | FloatDiv
    | FloatNeg
    | FloatLt
    | FloatLte
    | FloatEq
    | FloatGte
    | FloatGt
    | Int2Float
    | Float2Int
    | BoolNot
    | BoolAnd
    | BoolOr
    | BranchAbs Int
    | Branch Int
    | BranchTrue Int
    | BranchFalse Int
    | Halt
    | Load Int
    | GlobalLoad Int
    | Store Int
    | GlobalStore Int
    | LoadObject Int
    | GlobalLoadObject Int
    | StoreObject Int
    | GlobalStoreObject Int
    | Pop
    | Dup
    | DupX1
    | Swap
    | Call Int Int Int Int Int
    | Return Int
    | CallIn Int
    | PrintInt
    | PrintFloat
    | NewArray
    | ArrayLength
    | ArrayLoad
    | ArrayStore
      -- Special opcode:
    | Annotation AnnotationType


type AnnotationType
    = FunctionDef String
    | FunctionCall String


assemblySize : List OpCode -> Int
assemblySize list =
    List.map opcodeSize list
        |> List.sum


branchSize : Int
branchSize =
    2


opcodeSize : OpCode -> Int
opcodeSize oc =
    case oc of
        NoOp ->
            1

        IntConst _ ->
            2

        IntAdd ->
            1

        IntSub ->
            1

        IntMul ->
            1

        IntDiv ->
            1

        IntMod ->
            1

        IntNeg ->
            1

        IntLt ->
            1

        IntLte ->
            1

        IntEq ->
            1

        IntGte ->
            1

        IntGt ->
            1

        FloatConst _ ->
            2

        FloatAdd ->
            1

        FloatSub ->
            1

        FloatMul ->
            1

        FloatDiv ->
            1

        FloatNeg ->
            1

        FloatLt ->
            1

        FloatLte ->
            1

        FloatEq ->
            1

        FloatGte ->
            1

        FloatGt ->
            1

        Int2Float ->
            1

        Float2Int ->
            1

        BoolNot ->
            1

        BoolAnd ->
            1

        BoolOr ->
            1

        BranchAbs _ ->
            branchSize

        Branch _ ->
            branchSize

        BranchTrue _ ->
            branchSize

        BranchFalse _ ->
            branchSize

        Halt ->
            1

        Load _ ->
            2

        GlobalLoad _ ->
            2

        Store _ ->
            2

        GlobalStore _ ->
            2

        LoadObject _ ->
            2

        GlobalLoadObject _ ->
            2

        StoreObject _ ->
            2

        GlobalStoreObject _ ->
            2

        Pop ->
            1

        Dup ->
            1

        DupX1 ->
            1

        Swap ->
            1

        Call _ _ _ _ _ ->
            6

        Return _ ->
            2

        CallIn _ ->
            2

        PrintInt ->
            1

        PrintFloat ->
            1

        NewArray ->
            1

        ArrayLength ->
            1

        ArrayLoad ->
            1

        ArrayStore ->
            1

        Annotation _ ->
            0


encoder : List Int -> Encoder
encoder list =
    map signedInt list
        |> sequence


signedInt : Int -> Encoder
signedInt =
    signedInt32 LE


opcodeToEncoder : OpCode -> Encoder
opcodeToEncoder oc =
    case oc of
        NoOp ->
            encoder []

        IntConst val ->
            encoder [ 1, val ]

        IntAdd ->
            signedInt 2

        IntSub ->
            signedInt 3

        IntMul ->
            signedInt 4

        IntDiv ->
            signedInt 5

        IntMod ->
            signedInt 6

        IntNeg ->
            signedInt 7

        IntLt ->
            signedInt 8

        IntLte ->
            signedInt 9

        IntEq ->
            signedInt 10

        IntGte ->
            signedInt 11

        IntGt ->
            signedInt 12

        FloatConst val ->
            sequence
                [ signedInt 13
                , float32 LE val
                ]

        FloatAdd ->
            signedInt 14

        FloatSub ->
            signedInt 15

        FloatMul ->
            signedInt 16

        FloatDiv ->
            signedInt 17

        FloatNeg ->
            signedInt 18

        FloatLt ->
            signedInt 19

        FloatLte ->
            signedInt 20

        FloatEq ->
            signedInt 21

        FloatGte ->
            signedInt 22

        FloatGt ->
            signedInt 23

        Int2Float ->
            signedInt 24

        Float2Int ->
            signedInt 25

        BoolNot ->
            signedInt 26

        BoolAnd ->
            signedInt 27

        BoolOr ->
            signedInt 28

        BranchAbs count ->
            encoder [ 29, count ]

        Branch count ->
            encoder [ 30, count ]

        BranchTrue count ->
            encoder [ 31, count ]

        BranchFalse count ->
            encoder [ 32, count ]

        Halt ->
            signedInt 33

        Load index ->
            encoder [ 34, index ]

        GlobalLoad index ->
            encoder [ 35, index ]

        Store index ->
            encoder [ 36, index ]

        GlobalStore index ->
            encoder [ 37, index ]

        LoadObject index ->
            encoder [ 38, index ]

        GlobalLoadObject index ->
            encoder [ 39, index ]

        StoreObject index ->
            encoder [ 40, index ]

        GlobalStoreObject index ->
            encoder [ 41, index ]

        Pop ->
            signedInt 42

        Dup ->
            signedInt 43

        DupX1 ->
            signedInt 44

        Swap ->
            signedInt 45

        Call index argPrimCount localPrimCount argObjCount localObjCount ->
            encoder [ 46, index, argPrimCount, localPrimCount, argObjCount, localObjCount ]

        Return index ->
            encoder [ 47, index ]

        CallIn index ->
            encoder [ 48, index ]

        PrintInt ->
            signedInt 49

        PrintFloat ->
            signedInt 50

        NewArray ->
            signedInt 51

        ArrayLength ->
            signedInt 52

        ArrayLoad ->
            signedInt 53

        ArrayStore ->
            signedInt 54

        Annotation _ ->
            encoder []
