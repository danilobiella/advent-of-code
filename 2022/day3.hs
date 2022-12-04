import Data.Char (ord)
import Data.List.Split (chunksOf)

type Items = String

type Rucksack = (Items, Items)

type Group = [String]

main = do
  fileContent <- readFile "input/day3_input.txt"
  print $ partB fileContent

-- Parsing

parseInput :: String -> [Rucksack]
parseInput = map splitInHalf . lines
  where
    splitInHalf :: String -> Rucksack
    splitInHalf xs
      | even (length xs) = (take half xs, take half (reverse xs))
      | otherwise = error "Item is not of even length"
      where
        half = length xs `div` 2

parseInputpartB :: String -> [Group]
parseInputpartB = chunksOf 3 . lines

-- Core

intersects :: Eq a => [[a]] -> [a]
intersects = foldl1 intersectsTwo

intersectsTwo :: Eq a => [a] -> [a] -> [a]
intersectsTwo xs ys = filter (`elem` ys) xs

priority :: Char -> Int
priority x
  | asciiValue >= 97 = asciiValue - 96
  | asciiValue >= 65 = asciiValue - 65 + 27
  | otherwise = error "Invalid character"
  where
    asciiValue = ord x

-- Parts
partA :: String -> Int
partA = sum . map (priority . (!! 0) . uncurry intersectsTwo) . parseInput

--
partB :: String -> Int
partB = sum . map (priority . (!! 0) . intersects) . parseInputpartB
