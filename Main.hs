module Main where

import System.IO

main =  putStr "Provide name :: " >> hFlush stdout >> getLine >>= \l -> putStr ("Hello " ++ l ++ "!\n")
