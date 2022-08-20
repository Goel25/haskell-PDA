module Types where

newtype Program = Program [State]
data State = Accepting String [Case] | Rejecting String [Case]
data Case = Case { tape :: Char, stack :: Char, statements :: [Statement] }
data Statement = Push Char
               | Pop
               | Goto String
               deriving Show
showListWithSeparator :: Show a => [a] -> String -> String
showListWithSeparator inp sep = foldl (\acc x -> acc ++ sep ++ show x) "" inp

instance Show Program where
  show (Program states) = showListWithSeparator states "\n"
instance Show State where
  show (Accepting name cases) = name ++ "! {" ++ showListWithSeparator cases "\n" ++ "\n}"
  show (Rejecting name cases) = name ++ " {" ++ showListWithSeparator cases "\n" ++ "\n}"
instance Show Case where
  show (Case tape stack statements) = "  " ++
                  [tape,stack] ++
                  showListWithSeparator statements "\n    "

newtype Parser a = Parser {
            runParser :: [String] -> Either String (a, [String])
                        }

data Machine = RunningMachine Program String
             | AcceptedMachine
             | RejectedMachine

newtype MyStack = MyStack [Char]
  deriving Show

peek :: MyStack -> Char
peek (MyStack []) = '_' -- Indicates empty stack
peek (MyStack xs) = last xs -- Indicates empty stack

push :: MyStack -> Char -> MyStack
push (MyStack xs) x = MyStack (xs ++ [x])

pop :: MyStack -> MyStack
pop (MyStack xs) = MyStack $ go xs
  where
    go [] = [] -- TODO What if pop empty stack?
    go [x] = []
    go (x:xs) = x : go xs

isEmpty :: MyStack -> Bool
isEmpty (MyStack xs) = null xs
