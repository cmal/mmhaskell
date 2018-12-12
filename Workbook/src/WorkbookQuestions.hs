module WorkbookQuestions where

import qualified Data.Map as M

evens :: [a] -> [a]
evens [] = []
evens (x:[]) = []
evens (x:y:xs) = y:(evens xs)

addWhenMod3Is2 :: [Int] -> [Int]
addWhenMod3Is2 [] = []
addWhenMod3Is2 (x:xs) = case (mod x 3) of
  2 -> (x + 3):(addWhenMod3Is2 xs)
  _ -> addWhenMod3Is2 xs
  

reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (x:xs) = (reverse_ xs) ++ [x]

reverseAccum :: [a] -> [a]
reverseAccum = foldl (\xs x -> x:xs) []

specialMultiples :: [Int] -> [Int]
specialMultiples [] = []
specialMultiples (x:xs) = fmap (* x) [2,3,4] ++ specialMultiples xs

manyStrings :: [Int] -> [String] -> [String]
manyStrings [] _ = []
manyStrings _ [] = []
manyStrings (i:is) (s:ss) = (take i $ repeat s) ++ manyStrings is ss

addPairs :: [Int] -> [Int]
addPairs [] = []
addPairs [x] = []
addPairs (x:y:xs) = (x + y):addPairs xs

listToMap :: [Int] -> M.Map String Int
listToMap = M.fromList . listToPairs
  where listToPairs [] = []
        listToPairs (x:xs) = (show x, x):listToPairs xs

sumWithParity :: [Int] -> Int
sumWithParity [] = 0
sumWithParity (x:[]) = 3 * x
sumWithParity (x:y:xs) = 3 * x + 2 * y + sumWithParity xs

-- This function takes two arguments. The first is a list of integers
-- representing the heights of successive jumps. The second is a list
-- of tuples representing steps, each with a name, and an integer
-- representing how high it is ABOVE the previous step (not absolute
-- height). Return a tuple partitioning the step names into those
-- which you are able to climb given the series of jumps. One jump can
-- be used to climb multiple steps, but you can’t otherwise “save”
-- energy from past jumps to get over higher steps.

jumpingStairs :: [Int] -> [(String, Int)] -> ([String], [String])
jumpingStairs a b = go a b ([], map fst b)
  where go [] _ res = res
        go _ [] res = res
        go (j:jumps) steps@((name, height):stepsRemain) res =
          case j >= height of
            False -> go jumps steps res
            True -> go ((j - height):jumps) stepsRemain ((fst res) ++ [name], tail $ snd res)
  
