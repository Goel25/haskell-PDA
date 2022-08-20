module Main where

import System.IO

import Types
import RunParser
import SimulateMachine

main :: IO ()
main = print "a"

run :: String -> IO ()
run inp = do
    handle <- openFile "simpleMachine.pda" ReadMode
    contents <- hGetContents handle
    -- TODO Verify input doesn't have _ or *
    case (runParser programParser . tokenize) contents of
      Left err -> print err
      Right (p, _) -> print $ runMachine p inp
