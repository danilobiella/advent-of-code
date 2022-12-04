import Data.List (sort)
import Data.List.Split (splitOn)

main = do
  fileContent <- readFile "input/day1_input.txt"
  print $ partB fileContent

parseInput :: String -> [String]
parseInput = lines

partA :: String -> Int
partA = findTotalCaloriesOfTopN 1 . parseInput

partB :: String -> Int
partB = findTotalCaloriesOfTopN 3 . parseInput

findTotalCaloriesOfTopN :: Int -> [String] -> Int
findTotalCaloriesOfTopN n = sum . take n . reverse . sort . map (sum . map read) . splitOn [""]
