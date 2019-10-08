module Main where
import System.Environment

add :: Int -> Int -> Int
add a b = a + b

main :: IO ()
main = do putStrLn "What is your name?"
          name <- getLine
          putStrLn $ "Hello, " ++ name
