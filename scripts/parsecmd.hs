#!/usr/bin/env runhaskell

import System.Process
import System.Environment

repl :: Char -> Char
repl '|' = ' '
repl c = c

main :: IO ()
main = do
    args <- getArgs
    callCommand $ map repl (concat args)
