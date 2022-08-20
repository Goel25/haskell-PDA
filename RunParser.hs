{-# LANGUAGE LambdaCase #-}
module RunParser where

import Types
import Control.Monad
import Control.Exception

programParser :: Parser Program
programParser = Parser $ \inp -> do
  (state, afterStr) <- runParser stateParser inp
  (restStates, afterStr') <- if null afterStr
                                then Right ([], [])
                                else do
                                  (Program states, str) <- runParser programParser afterStr
                                  return (states, str)
  return (Program $ state : restStates, afterStr')

stateParser :: Parser State
stateParser = Parser $ \case
  (stateName:"{":inp) -> do
    (body, afterStr) <- runParser bodyParser inp

    if last stateName == '!' then Right (Accepting (init stateName) body, afterStr)
    else Right (Rejecting stateName body, afterStr)
      where
        bodyParser :: Parser [Case]
        bodyParser = Parser $ \inp -> do
          (caseExpr, afterStr) <- runParser caseParser inp
          (restCases, afterStr') <- if head afterStr == "}"
                                       then Right ([], tail afterStr)
                                       else runParser bodyParser afterStr
          return (caseExpr : restCases, afterStr')
  _ -> Left "Err not state"



caseParser :: Parser Case
caseParser = Parser $ \case
  ([tape, stack]:xs) -> do
                    (statements, afterStr) <- runParser statementsParser xs
                    return (Case tape stack statements, afterStr)
    where
      statementsParser :: Parser [Statement]
      statementsParser = Parser $ \inp ->
                    if null inp then Left "Each state must close with a goto"
                    else do
                      (statement, restStr) <- runParser statementParser inp
                      (restStatements, afterStr) <- case statement of
                                                      Goto state -> Right ([], restStr)
                                                      _ -> runParser statementsParser restStr
                      return (statement : restStatements, afterStr)
  _ -> Left "A case must begin with a tape and stack symbol"

statementParser :: Parser Statement
statementParser = go [pushParser, popParser, gotoParser]
  where
    go :: [Parser Statement] -> Parser Statement
    go [] = Parser $ \inp -> Left $ if null inp then "Unexpected EOF" else "Unrecognized statement " ++ head inp
    go (parser:otherParsers) = Parser $ \inp -> case runParser parser inp of
                                 Right success -> Right success
                                 Left "" -> runParser (go otherParsers) inp
                                 Left err -> Left err


pushParser :: Parser Statement
pushParser = Parser $ \case
                  ("push":[c]:afterStr) -> Right (Push c, afterStr)
                  ("push":afterStr) -> Left "Push requires a single stack symbol is pushed"
                  _ -> Left ""

popParser :: Parser Statement
popParser = Parser $ \case
                  ("pop":afterStr) -> Right (Pop, afterStr)
                  _ -> Left ""

gotoParser :: Parser Statement
gotoParser = Parser $ \case
                  ("goto":state:afterStr) -> Right (Goto state, afterStr)
                  _ -> Left ""

tokenize :: String -> [String]
tokenize str = join $ map (words . removeComment) (lines str)
        where
          removeComment :: String -> String
          removeComment line = snd $ foldl
                                (\acc x -> if fst acc || x == '#'
                                              then (True, snd acc)
                                              else (False, snd acc ++ [x])) (False, "") line
