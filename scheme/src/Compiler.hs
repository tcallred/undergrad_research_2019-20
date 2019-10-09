module Compiler
    ( compileProgram
    , Expr(Immediate)
    , Immediate(SNum)
    ) where


newtype Immediate = 
    SNum Int
    
    
newtype Expr =
    Immediate Immediate


compileProgram :: Expr -> [String]
compileProgram x =
    case x of
        Immediate immediate -> ("movl $" ++ immediateToString immediate ++ ", %eax") : ["ret"]


immediateToString :: Immediate -> String
immediateToString immediate =
    case immediate of
        SNum number -> show number