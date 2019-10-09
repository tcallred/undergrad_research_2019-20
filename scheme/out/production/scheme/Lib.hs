module Lib
    ( compileProgram,
      Expr(SNum)
    ) where

newtype Expr = SNum Int deriving (Show, Read)

-- Compile scheme to x86
compileProgram :: Expr -> [String]
compileProgram x = 
  case x of 
    SNum i -> "movl $" ++ show i ++ ", %eax" 
    
   : ["ret"]
