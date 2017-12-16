module Mastermind

import StdEnv, StdLib

positionalMatches :: [Int] [Int] -> Int
positionalMatches [] _ = 0
positionalMatches _ [] = 0
positionalMatches [x:xs] [y:ys]
 | x == y = 1 + (positionalMatches xs ys)
 | otherwise = positionalMatches xs ys

matches :: [Int] [Int] -> Int
matches ys xs = matchesH (sort ys) (sort xs)
  where
    matchesH [] _ = 0
    matchesH _ [] = 0
    matchesH [x:xs] [y:ys]
      | x > y = matchesH [x:xs] ys
      | x < y = matchesH xs [y:ys]
      | otherwise = 1 + (matchesH xs  ys)

readCode :: String -> Maybe [Int]
readCode str = readCodeH (fromString str)
  where
    readCodeH chars
      | and [isDigit c \\ c <- chars] && ((length chars) == 4) = Just (map digitToInt chars)
      | otherwise = Nothing

maybe :: (a -> b) b (Maybe a) -> b
maybe _ d Nothing = d
maybe f d (Just v) = f v

allMatches :: [Int] String -> (Int, Int)
allMatches a b = allMatchesHelper a (((maybe id []) o readCode) b)
  where
    allMatchesHelper nums1 nums2 = ((matches nums1 nums2) - posMatches, posMatches)
      where
        posMatches = positionalMatches nums1 nums2


Start = (and result,zip2 [1..] (result), tests)
  where
    result = map and tests

tests = [
          positionalMatches_test
         ,matches_test
         ,readCode_test
         ,maybe_test
         ,allMatches_test
        ]

positionalMatches_test =
  [ positionalMatches [4,2,7,1] [1,2,3,4] == 1
  , positionalMatches [9,3,0,5] [5,6,7,8] == 0
  , positionalMatches [6,6,6,1] [6,6,5,1] == 3
  ]

matches_test =
  [ matches [4,2,7,1] [1,2,3,4] == 3
  , matches [9,3,0,5] [5,6,7,8] == 1
  , matches [6,6,6,1] [6,6,5,1] == 3
  , matches [5,8,7,9] [9,9,7,8] == 3
  ]

readCode_test =
  [ readCode "1234"  == Just [1,2,3,4]
  , readCode "12345" == Nothing
  , readCode "123a"  == Nothing
  ]

maybe_test =
  [ maybe ((+) 10) 7 Nothing  == 7
  , maybe ((+) 10) 7 (Just 5) == 15
  ]

allMatches_test =
  [ allMatches [4,2,7,1] "1234" == (2, 1)
  , allMatches [9,3,0,5] "1234" == (1, 0)
  , allMatches [9,3,0,5] "123a" == (0, 0)
  ]