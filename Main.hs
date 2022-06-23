{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO
import Control.Monad
import Control.Exception

data LOC = Push Char
         | Pop
         | Accept
         | Reject
         | Body [LOC]
         | CaseExpr [LOC]
         | Case { tape :: Char, stack :: Char, expression :: LOC }
         | Err String
       deriving Show

newtype Parser = Parser {
            runParser :: [String] -> Either String (LOC, [String])
                        }

main :: IO ()
main = do
    handle <- openFile "simpleMachine.pda" ReadMode
    contents <- hGetContents handle
    print $ (parseFull . tokenize) contents

parseFull :: [String] -> Either String [LOC]
parseFull [] = Right []
parseFull str = do
                  (loc, restStr) <- runParser parse str
                  rest <- parseFull restStr
                  return $ loc : rest

allParsers :: [Parser]
allParsers = [parsePop, parsePush, parseCaseExpr, parseReject, parseAccept]

parse :: Parser
parse = Parser $ \str -> go str allParsers
  where
    go :: [String] -> [Parser] -> Either String (LOC, [String])
    go str [] = Left $ "Could not parse " ++ head str
    go str (parser:xs) = case runParser parser str of
                           Right x -> Right x
                           Left "" -> go str xs
                           Left err -> Left err


parseCaseExpr :: Parser
parseCaseExpr = Parser $ \case
                  ("case":"{":xs) -> do
                        (body, afterStr) <- runParser parseBody xs
                        return $ case body of
                            Body lines -> (CaseExpr lines, afterStr)
                            x -> (CaseExpr [x], afterStr)
                    where
                      parseBody :: Parser
                      parseBody = Parser $ \case
                          ("}":xs) -> Right (Body [], xs)
                          xs -> do
                                  (loc, restStr) <- runParser parseCase xs
                                  (restLoc, afterStr) <- runParser parseBody restStr
                                  return (appendToBody loc restLoc, afterStr)

                      appendToBody :: LOC -> LOC -> LOC
                      appendToBody (Body xs) (Body ys) = Body $ xs ++ ys
                      appendToBody x (Body xs) = Body $ x : xs
                      appendToBody (Body xs) x = Body $ xs ++ [x]
                      appendToBody x y = Body [x, y]

                      parseCase :: Parser
                      parseCase = Parser $ \case
                                    ([tape, stack]:restStr) -> do
                                              (loc, restStr) <- runParser parse restStr
                                              return (Case tape stack loc, restStr)
                                    ([a]:_) -> Left $
                                              "Case expression '"
                                              ++ [a]
                                              ++ "' must have a tape and stack char"
                                    (a:_) -> Left $
                                              "Not a valid case expression starting with '"
                                              ++ a
                                              ++ "' must have just a tape and stack char"
                                    _ -> Left "Not a valid case expression"
                  _ -> Left ""

parsePush :: Parser
parsePush = Parser $ \case
              ("push":[c]:xs) -> Right (Push c, xs)
              ("push":chars:xs) -> Left $ "Only a single char may be pushed, not '" ++ chars ++ "'"
              ["push"] -> Left "Unexpected end of input after push"
              _ -> Left ""

parsePop :: Parser
parsePop = Parser $ \case
              ("pop":xs) -> Right (Pop, xs)
              _ -> Left ""

parseAccept :: Parser
parseAccept = Parser $ \case
              ("accept":xs) -> Right (Accept, xs)
              _ -> Left ""

parseReject :: Parser
parseReject = Parser $ \case
              ("reject":xs) -> Right (Reject, xs)
              _ -> Left ""

tokenize :: String -> [String]
tokenize str = join $ map (words . removeComment) (lines str)
        where
          removeComment :: String -> String
          removeComment line = snd $ foldl
                                (\acc x -> if fst acc || x == '#'
                                              then (True, snd acc)
                                              else (False, snd acc ++ [x])) (False, "") line
