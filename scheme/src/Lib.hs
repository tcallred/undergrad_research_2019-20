module Lib
    ( compileProgram
    ) where


-- Compile scheme to x86
compileProgram :: String -> String
compileProgram x = "movl $" ++ x ++ ", %eax" ++ "\n" ++ "ret"
