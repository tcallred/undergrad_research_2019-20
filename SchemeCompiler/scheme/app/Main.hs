module Main where

import Compiler


main :: IO ()
main = compileProgram (Primcall (Sub1 (Immediate (Integer 4))))
