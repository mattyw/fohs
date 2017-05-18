module Main where

import Model.Model

main :: IO ()
main = repl []

repl :: [Int] -> IO()
repl stk = do
    putStr "> "
    line <- getLine
    if line == ":q"
        then putStrLn $ show stk
        else do
            let newStk = run stk (parse [line])
            putStrLn $ show newStk
            repl newStk
