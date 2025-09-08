#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall #-}

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import System.Directory (removeFile)
import System.Directory.Internal.Prelude (getArgs)

defaultFile :: FilePath
defaultFile = "/home/ramak/projects/mlscript/hkmc2/shared/src/auto_test.mls"

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Nothing,
  autoAddHistory = False
}

writeTest :: FilePath -> [String] -> String -> IO ()
writeTest fname flags curpr = do
  let contents = unlines $ map (':':) flags ++ [curpr]
  writeFile fname contents

formatContents :: [String] -> [String]
formatContents [] = []
formatContents ((':':flag1):(':':flag2):rest) = formatContents $ ((':':flag1) ++ " " ++ (':':flag2)) : rest
formatContents [':':flags, pr] = ["", ':':flags, "", "> " ++ pr, ""]
formatContents ((':':flags):pr:rest) = "" : (':':flags) : "" : ("> " ++ pr) : "" : rest ++ [""]
formatContents [pr] = ["", "> " ++ pr] ++ [""]
formatContents (pr:rest) = "" : ("> " ++ pr) : "" : rest ++ [""]

readTest :: FilePath -> IO ()
readTest fname = do
  !contents <- readFile fname
  putStr $ unlines $ formatContents (lines contents)

replace :: Char -> Char -> String -> String
replace from to = map (\ x -> if x == from then to else x)

split :: Char -> String -> String -> [String]
split _ acc [] = [acc]
split c acc (c':rest)
  | c == c' = acc : split c "" rest
  | otherwise = split c (acc ++ [c']) rest

main :: IO ()
main = do
  args <- getArgs
  let fname = case args of
        [] -> defaultFile
        (str:_) -> str
      sfname = last (split '/' "" fname)
      loop :: [String] -> String -> Bool -> InputT IO ()
      loop flags curpr write = do
        liftIO $ do
          when write $ writeTest fname (map (replace '#' ' ') flags) curpr >> threadDelay 200000
          readTest fname
        prompt <- getInputLine $ "(" ++ sfname ++ ") # "
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
          Just "exit" -> liftIO (removeFile fname)
          Just "" -> loop flags curpr False
          Just pr -> loop flags pr True
          _ -> loop flags curpr False
  writeFile fname ""
  runInputT settings (loop [] "" False)
