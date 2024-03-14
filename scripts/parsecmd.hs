#!/usr/bin/env runhaskell

import System.Process
import System.Environment

parse :: String -> String -> String
parse "" arg = ""
parse (';' : ';' : cmd) arg  = ';' : parse cmd arg
parse (';' : cmd) arg = ' ' : parse cmd arg
parse ('%' : cmd) arg = (" " ++ arg ++ " ") ++ parse cmd arg
parse (c : cmd) arg = c : parse cmd arg

main :: IO ()
main = do
    args <- getArgs
    callCommand $ parse (args !! 0) (args !! 1)
