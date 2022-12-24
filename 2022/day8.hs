import Data.List (transpose)
import Data.List.Split (chunksOf)

type Forest = [[Int]]

main = do
  fileContent <- readFile "input/day8_input.txt"
  putStrLn "PartA:"
  print (partA fileContent)
  putStrLn "PartB:"
  print (partB fileContent)

-- Parsing

parseInput :: String -> Forest
parseInput = map (map read . chunksOf 1) . lines

-- Core
countVisibleTrees :: Forest -> Int
countVisibleTrees = length . filter (/= False) . concat . findVisibleTrees

findVisibleTrees :: Forest -> [[Bool]]
findVisibleTrees =
  map (map or . transpose)
    . transpose
    . invertRotations
    . map findVisibleOneSide
    . makeRotations

viewingDistance :: Forest -> [[Int]]
viewingDistance =
  map (map product . transpose)
    . transpose
    . invertRotations
    . map viewingDistanceOneSide
    . makeRotations

findVisibleOneSide :: Forest -> [[Bool]]
findVisibleOneSide = map (map snd . tail . scanl findLine (-1, True))
  where
    findLine (x, _) y = if x >= y then (x, False) else (y, True)

viewingDistanceOneSide :: Forest -> [[Int]]
viewingDistanceOneSide = map distOneline
  where
    distOneline b = zipWith (curry (boh . uncurry take)) [0 ..] (map (sequence ((<) <$> b)) b)

boh :: [Bool] -> Int
boh xs = 1 + boh' (reverse xs)
  where
    boh' [] = -1
    boh' [True] = 0
    boh' (False : xs) = 0
    boh' (True : xs) = 1 + boh' xs

makeRotations :: [[a]] -> [[[a]]]
makeRotations forest =
  [ forest,
    map reverse forest,
    transpose forest,
    map reverse $ transpose forest
  ]

invertRotations :: [[[a]]] -> [[[a]]]
invertRotations [f1, f2, f3, f4] =
  [ f1,
    map reverse f2,
    transpose f3,
    transpose $ map reverse f4
  ]
invertRotations _ = error "needs to be a 4 element list"

-- Parts
partA :: String -> Int
partA = countVisibleTrees . parseInput

-- --
partB :: String -> Int
partB = maximum . map maximum . viewingDistance . parseInput
