module TypeCheck 
    () where


import Data.Map


import MuKanren


-- AST for our little language
data Expr = 
    Lambda Var Expr
    | Expr Expr
    | Var Var
    | Const Const
    | Operator Op Expr Expr
    deriving (Eq, Show)


newtype Var = Sym String deriving (Eq, Show)


data Const = 
    Str String
    | Int Int
    | Bool Bool
    | Float Float
    deriving (Eq, Show)
    

data Op = 
    Plus
    | Minus
    | Multiply 
    deriving (Eq, Show)


data Type =
    Single B
    | Arrow Type Type
    deriving(Eq, Show)


data B = 
    IntT
    | BoolT
    | FloatT
    | StringT
    deriving(Eq, Show)


type TypeEnv = Data.Map Var Type 


data Term = 
    TypeEnv TypeEnv 
    | Type Type
    | Expr Expr

