import Data.List.Split (splitOn)

data RockPaperScissor = Rock | Paper | Scissor deriving (Show, Eq)
type Round = (RockPaperScissor, RockPaperScissor)

loser :: RockPaperScissor -> RockPaperScissor
loser Rock = Scissor
loser Paper = Rock
loser Scissor = Paper 

winner :: RockPaperScissor -> RockPaperScissor
winner Rock = Paper
winner Paper = Scissor
winner Scissor = Rock 

main = do
    fileContent <- readFile "input/day2_input.txt"
    print $ partB fileContent

-- Parsing 
parseInput :: String -> [Round]
parseInput = map parseOneLine . lines 
  where
  parseOneLine :: String -> Round 
  parseOneLine = tuplify . map readRPS . splitOn " "
  
  tuplify :: [a] -> (a, a)
  tuplify [x1, x2] = (x1, x2)
  tuplify _ = error "Not a 2 element list"
  
  readRPS :: String -> RockPaperScissor
  readRPS "A" = Rock
  readRPS "B" = Paper 
  readRPS "C" = Scissor
  readRPS "X" = Rock
  readRPS "Y" = Paper 
  readRPS "Z" = Scissor
  readRPS _ = error "Not a valid RockPaperScissor string"

-- Core

totalScore :: Round -> Int
totalScore r = outcome r + shapeValue r
  where
  outcome :: Round -> Int
  outcome (x, y)
    | loser x == y  = 0
    | winner x == y = 6
    | otherwise     = 3

  shapeValue :: Round -> Int
  shapeValue (_, Rock)    = 1
  shapeValue (_, Paper)   = 2
  shapeValue (_, Scissor) = 3

partBStrategy :: Round -> Round
partBStrategy (x, Paper) = (x, x)
partBStrategy (x, Rock) = (x, loser x)
partBStrategy (x, Scissor) = (x, winner x)

-- Parts
partA :: String -> Int
partA = sum . map totalScore . parseInput

partB :: String -> Int
partB = sum . map (totalScore . partBStrategy) . parseInput


