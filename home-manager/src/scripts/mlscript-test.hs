#!/usr/bin/env runhaskell

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

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
  let contents = unlines $ ["%%%"] ++ map (':':) flags ++ [curpr] ++ ["%%%"]
  writeFile file contents
  threadDelay 1
  !newcts <- readFile file
  putStrLn newcts


loop :: [String] -> String -> InputT IO ()
loop flags curpr = do
  liftIO $ writeTest flags curpr
  prompt <- getInputLine "(test) # "
  case prompt of
    Just (':':flag) -> loop (flag : flags) curpr
    Just ('/':flag) -> loop (filter (/= flag) flags) curpr
    Just pr -> loop flags pr
    _ -> loop flags curpr

main :: IO ()
main = runInputT settings (loop [] "")
