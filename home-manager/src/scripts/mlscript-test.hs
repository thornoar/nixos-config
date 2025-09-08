#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import System.Directory (removeFile)

file :: FilePath
file = "/home/ramak/projects/mlscript/hkmc2/shared/src/auto_test.mls"

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Nothing,
  autoAddHistory = False
}

writeTest :: [String] -> String -> IO ()
writeTest flags curpr = do
  let contents = unlines $ map (':':) flags ++ [curpr]
  writeFile file contents

modifyContents :: [String] -> [String]
modifyContents [] = []
modifyContents [pr] = ["", "> " ++ pr] ++ [""]
modifyContents ((':':flag1):(':':flag2):rest) = modifyContents $ ((':':flag1) ++ " " ++ (':':flag2)) : rest
modifyContents ((':':flags):pr:rest) = "" : (':':flags) : "" : ("> " ++ pr) : "" : rest ++ [""]
modifyContents (pr:rest) = "" : ("> " ++ pr) : "" : rest ++ [""]

readTest :: IO ()
readTest = do
  !contents <- readFile file
  putStr $ unlines $ modifyContents (lines contents)

replace :: Char -> Char -> String -> String
replace from to = map (\ x -> if x == from then to else x)

exit :: IO ()
exit = removeFile file

loop :: [String] -> String -> Bool -> InputT IO ()
loop flags curpr write = do
  liftIO $ do
    when write $ writeTest (map (replace '#' ' ') flags) curpr >> threadDelay 200000
    readTest
  prompt <- getInputLine "(test) # "
  let modifyFlags :: String -> [String] -> [String]
      modifyFlags pr = case pr of
        "//" -> const []
        ':':flag -> (flag :)
        '/':flag -> filter (/= flag)
        _ -> id
      foldr' :: (a -> b -> b) -> b -> [a] -> b
      foldr' _ !b [] = b
      foldr' f !b (a:as) = foldr' f (f a b) as

  case prompt of
    Just pr@(c:_)
      | c `elem` [':', '/'] -> loop (foldr' modifyFlags flags (words pr)) curpr True
    Just "exit" -> liftIO exit
    Just "" -> loop flags curpr False
    Just pr -> loop flags pr True
    _ -> loop flags curpr False

main :: IO ()
main = writeFile file "" >> runInputT settings (loop [] "" False)
