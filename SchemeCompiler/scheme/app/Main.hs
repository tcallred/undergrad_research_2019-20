module Main where

import Compiler


main :: IO ()
main = do
    putStrLn ".global scheme_entry"
    putStrLn ".type scheme_entry, @function"
    putStrLn "scheme_entry:"
    putStrLn . unlines $ compileProgram (Immediate (Integer 42))
