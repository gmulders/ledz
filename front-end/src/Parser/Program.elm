module Parser.Program exposing (LampProgram, programParser)

import Parser exposing (..)
import Parser.Extra exposing (..)
import Parser.Extras exposing (..)
import Parser.Expression exposing (..)
import Set

type Function
  = Function String (List Variable) Type (List Variable) (List Statement)

type Variable
  = Variable String Type

type LampProgram
  = LampProgram (List Variable) (List Function)

type Type
  = Integer
  | Float
  | IntegerArray
  | FloatArray

type Statement
  = ExpressionStatement Expr
  | IfStatement Expr (List Statement)
  | IfElseStatement Expr (List Statement) (List Statement)
  | WhileStatement Expr (List Statement)
  | Assignment String Expr
  | ArrayAssignment String Expr Expr
  | CreateArrayAssignment String Type Expr
  | EchoStatement Expr
  | ReturnStatement Expr

type Expr
  = IfExpr
  | CallExpr
  | VariableExpr Variable
  | IntExpr Int
  | FloatExpr Float
  | IndexExpr
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
    , reserved = Set.fromList []
    }

typea : Parser Type
typea =
  oneOf
    [ succeed Integer |. keyword "Int"
    , succeed Float |. keyword "Float"
    , succeed IntegerArray |. keyword "IntegerArray"
    , succeed FloatArray |. keyword "FloatArray"
    ]

variable : Parser Variable
variable =
  succeed Variable
    |= identifier
    |. spaces
    |. symbol ":"
    |. spaces
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
    , trailing = Forbidden -- demand a trailing semi-colon
    }

expression : Parser Expr
expression =
  problem "expr"

assignment : Parser Statement
assignment =
  succeed Assignment
    |= identifier
    |. spaces
    |. symbol "="
    |. spaces
    |= expression

statement : Parser Statement
statement = 
  oneOf
    [ assignment
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
    |. spaces
    |= parameterList
    |. spaces
    |. symbol ":"
    |. spaces
    |= typea
    |. spaces
    |. symbol "{"
    |. spaces
    |= declarations
    |. spaces
    |= statements
    |. spaces
    |. symbol "}"


functions : Parser (List Function)
functions =
  some function |> map (\(v, vl) -> v :: vl)

programParser : Parser LampProgram
programParser =
  succeed LampProgram
    |= declarations
    |= functions
    |. end



-- assoc : (a -> b -> c) -> (c -> d) -> a -> b -> d
-- assoc =
--   (>>) (>>)

compose3 : (a -> b -> c) -> (c -> d) -> a -> b -> d
compose3 f g l r =
  g (f l r)

operators : OperatorTable Expr
operators =
    [ --[ prefixOperator negate (symbol "-"), prefixOperator identity (symbol "+") ]
     [ infixOperator (compose3 MultiplyExpr BinaryExpression) (symbol "*") AssocLeft ]
--    , [ infixOperator (+) (symbol "+") AssocLeft, infixOperator (-) (symbol "-") AssocLeft ]
    ]

term : Parser Expr
term =
    oneOf
        [ parens <| lazy (\_ -> expr)
        , succeed IntExpr
            |= int
            |. spaces
        , succeed FloatExpr
            |= float
            |. spaces
        ]

expr : Parser Expr
expr =
    buildExpressionParser operators (lazy <| \_ -> term)