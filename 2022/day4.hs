import Data.List.Split (splitOn)
import Data.Set (difference, empty, fromList, union)

main = do
  fileContent <- readFile "input/day4_input.txt"
  print $ partB fileContent

-- Parsing

parseInput :: String -> [[[Int]]]
parseInput = map (map (map read . splitOn "-") . splitOn ",") . lines

-- Core

makeRanges :: [Int] -> [Int]
makeRanges [x, y] = [x .. y]
makeRanges _ = error "Nonvalid pattern"

isContained :: Ord a => [a] -> [a] -> Bool
isContained xs ys = (setX `union` setY == setX) || (setY `union` setX == setY)
  where
    setX = fromList xs
    setY = fromList ys

isOverlap :: Ord a => [a] -> [a] -> Bool
isOverlap xs ys = ((setX `difference` setY) /= setX) || ((setY `difference` setX) /= setY)
  where
    setX = fromList xs
    setY = fromList ys

applyToFirstTwo :: (a -> a -> b) -> [a] -> b
applyToFirstTwo f xs = f (xs !! 0) (xs !! 1)

-- Parts
partA :: String -> Int
partA = sum . map (fromEnum . applyToFirstTwo isContained . map makeRanges) . parseInput

-- --
partB :: String -> Int
partB = sum . map (fromEnum . applyToFirstTwo isOverlap . map makeRanges) . parseInput 
