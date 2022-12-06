import Data.List (tails)
import Data.Set (fromList, size)

main = do
  fileContent <- readFile "input/day6_input.txt"
  putStrLn "PartA:"
  print (partA fileContent)
  putStrLn "PartB:"
  print (partB fileContent)

-- Core

findPacketMarker :: String -> Int
findPacketMarker = findMarker 4

findMessageMarker :: String -> Int
findMessageMarker = findMarker 14

findMarker :: Int -> String -> Int
findMarker markerSize buffer =
  fst . head $
    filter (isMarker markerSize . snd) $
      zip
        [markerSize ..]
        ( map (take markerSize) $
            tails buffer
        )


isMarker :: Int -> String -> Bool
isMarker n xs = size (fromList xs) == n

-- Parts
partA :: String -> Int
partA = findPacketMarker 

-- --
partB :: String -> Int
partB = findMessageMarker 
