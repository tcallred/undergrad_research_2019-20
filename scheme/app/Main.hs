module Main where

import Compiler


main :: IO ()
main = putStrLn . unlines $ compileProgram (Immediate (SNum 42))
