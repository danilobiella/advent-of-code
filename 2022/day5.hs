import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)

type Crate = Char

type Stack = [Crate]

type Supplies = [(Int, Stack)]

type Move = (Int, Int, Int)

type Moves = [Move]

main = do
  fileContent <- readFile "input/day5_input.txt"
  print ("PartA: " <> partA fileContent <> " PartB: " <> partB fileContent)

-- Parsing

parseInput :: String -> (Supplies, Moves)
parseInput input = (parseSupplies supplies, map parseMove moves)
  where
    (supplies, _ : moves) = break (== "") $ lines input

parseSupplies :: [String] -> Supplies
parseSupplies = map ((\xs -> (read [last xs], init xs)) . parseStack) . transpose . map (chunksOf 4)
  where
    parseStack [] = []
    parseStack ("   " : xs) = parseStack xs
    parseStack ("    " : xs) = parseStack xs
    parseStack (('[' : c : ']' : _) : xs) = c : parseStack xs
    parseStack ((' ' : c : _) : xs) = c : parseStack xs
    parseStack _ = error "Invalid pattern"

parseMove :: String -> Move
parseMove move = (read n, read fromN, read toN) --FIXME Bring read outside
  where
    [_, n, _, fromN, _, toN] = splitOn " " move

-- Core

applyMoveGeneral :: (Stack -> Stack) -> Supplies -> Move -> Supplies
applyMoveGeneral stackOrderFunc supplies (n, m, p) = map boh supplies
  where
    boh (i, xs)
      | i == m = (i, drop n xs)
      | i == p = (i, valueToMove ++ xs)
      | otherwise = (i, xs)
    valueToMove = stackOrderFunc $ reverse $ take n $ snd $ head $ filter ((== m) . fst) supplies

applyMove9000 :: Supplies -> Move -> Supplies
applyMove9000 = applyMoveGeneral id

applyMove9001 :: Supplies -> Move -> Supplies
applyMove9001 = applyMoveGeneral reverse

-- Parts
partA :: String -> String
partA input = map (head . snd) $ foldl applyMove9000 supplies moves
  where
    (supplies, moves) = parseInput input

-- --
partB :: String -> String
partB input = map (head . snd) $ foldl applyMove9001 supplies moves
  where
    (supplies, moves) = parseInput input
