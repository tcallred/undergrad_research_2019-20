module Main where
import Lib

main :: IO ()
main = putStrLn.unlines $ compileProgram (SNum 42)
 


