module SimulateMachine where

import Types

runMachine :: Program -> String -> String
runMachine program inp = case go inp "start" (MyStack []) of
                     Left err -> "Error: " ++ err
                     Right (finalState, finalStack) ->
                       case getStateOfProgram program finalState of
                         Right (Accepting _ _) ->
                           if isEmpty finalStack
                              then "Accept"
                              else "Reject (non-empty stack)"
                         Right (Rejecting _ _) -> "Reject"
                         Left err -> "Error in final state " ++ err
  where
    go :: String -> String -> MyStack -> Either String (String, MyStack)
    go [] curState stack = Right (curState, stack)
    go (x:xs) curState stack = do
          curState <- getStateOfProgram program curState
          curCase <- getCaseOfState curState (x, peek stack)
          let (stack', state') = runStatements (statements curCase) stack
          go xs state' stack'

    runStatements :: [Statement] -> MyStack -> (MyStack, String)
    runStatements (Goto x : xs) s = (s, x) --Ends with goto
    runStatements (Push c : xs) s = runStatements xs (push s c)
    runStatements (Pop : xs) s = runStatements xs (pop s)
    runStatements [] s = (s, "") -- This should never happen



getStateOfProgram :: Program -> String -> Either String State
getStateOfProgram (Program []) name = Left $ "No state with name " ++ name
getStateOfProgram (Program (x:xs)) name = let stateName = case x of
                                               Accepting stateName cases -> stateName
                                               Rejecting stateName cases -> stateName
                                      in if stateName == name
                                            then Right x
                                            else getStateOfProgram (Program xs) name

getCaseOfState :: State -> (Char, Char) -> Either String Case
getCaseOfState s (tChar, sChar) =
      go (case s of
            Accepting _ x -> x
            Rejecting _ x -> x)
      where
        go [] = Left $ "In state: " ++
                    (case s of Accepting n _ -> n
                               Rejecting n _ -> n)
                    ++ ", No case for tape: " ++ [tChar] ++ " stack: " ++ [sChar]
        go (x:xs) =
          if (tape x == tChar || tape x == '*') && (stack x == sChar || stack x == '*')
            then Right x
            else go xs

