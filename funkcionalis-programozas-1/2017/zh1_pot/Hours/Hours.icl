module Hours

import StdEnv, StdLib

:: Data :== [(String, Int)]

total :: Data -> Int
total xs = foldr (\(name, hour) s -> s + hour) 0 xs

atLeast20Hours :: Data -> Int
atLeast20Hours xs = length (filter (atLeastXHour 20) xs)

query :: Data -> [String]
query xs = take 3 (sort (map fst (filter (atLeastXHour 5) xs)))

findKeyWithDefault :: (String -> Bool) Int Data -> Int
findKeyWithDefault fv def [] = def
findKeyWithDefault fv def [(name, hour):ds]
  | fv name = hour
  | otherwise = findKeyWithDefault fv def ds

hoursFor :: Data String -> Int
hoursFor xs language = findKeyWithDefault (\(name) -> name == language) 0 xs

diffData :: Data Data -> Data
diffData xs ys = [ (name, (hoursFor xs name) - hour) \\ (name, hour) <- ys]

planSucceeded :: Data Data -> Bool
planSucceeded worked plan = and (map (atLeastXHour 0) (diffData worked plan))

atLeastXHour :: Int (String, Int) -> Bool
atLeastXHour min (name, hour) = hour >= min

Start = (and result,zip2 [1..] (result), tests)
  where
    result = map and tests

tests = [ total_test
  , atLeast20Hours_test
  , query_test
  , findKeyWithDefault_test
  , hoursFor_test
  , diffData_test
  , planSucceeded_test
  ]

total_test :: [Bool]
total_test =
  [ total []
    == 0
  , total [("Java", 10)]
    == 10
  , total [("Java", 10), ("Python", 5), ("Haskell", 25)]
    == 40
  ]

atLeast20Hours_test :: [Bool]
atLeast20Hours_test =
  [ atLeast20Hours []
    == 0
  , atLeast20Hours [("Haskell", 25)]
    == 1
  , atLeast20Hours [("Java", 10), ("Agda", 18)]
    == 0
  , atLeast20Hours [("Java", 10), ("Haskell", 30), ("Agda", 18), ("Go", 25)]
    == 2
  ]

query_test :: [Bool]
query_test =
  [ query []
    == []
  , query [("C", 10), ("Agda", 4), ("Java", 30), ("Haskell", 20), ("Go", 25)]
    == ["C", "Go", "Haskell"]
  , query  [("C", 3), ("Agda", 4), ("Java", 2), ("Haskell", 20), ("Go", 25)]
    == ["Go", "Haskell"]
  ]

findKeyWithDefault_test :: [Bool]
findKeyWithDefault_test =
  [ findKeyWithDefault (\_ -> True) 0 []
    == 0
  , findKeyWithDefault (\_ -> True) 0 [("C", 10), ("Java", 20)]
    == 10
  , findKeyWithDefault (\_ -> False) 0 [("C", 10), ("Java", 20)]
    == 0
  , findKeyWithDefault (\k -> k == "Go") 0 [("C", 10), ("Go", 40), ("Java", 60)]
    == 40
  ]

hoursFor_test :: [Bool]
hoursFor_test =
  [ hoursFor [] "Java"
    == 0
  , hoursFor [("Java", 20)] "Java"
    == 20
  , hoursFor [("Java", 20)] "Haskell"
    == 0
  , hoursFor [("Java", 20), ("Haskell", 25), ("C", 5)] "Haskell"
    == 25
  ]

diffData_test :: [Bool]
diffData_test =
  [ diffData [] []
    == []
  , diffData [("Java", 10)] []
    == []
  , diffData [] [("Java", 10)]
    == [("Java", ~10)]
  , sort (diffData [("Go", 5), ("Java", 20), ("C", 1)]
                   [("Java", 15), ("Haskell", 30)])
    == [("Haskell", ~30), ("Java", 5)]
  ]

planSucceeded_test :: [Bool]
planSucceeded_test =
  [ planSucceeded [("Java", 10), ("Haskell", 20), ("C", 10)]
                  [("Haskell", 15), ("Agda", 30)]
    == False
  , planSucceeded [("Java", 10), ("Haskell", 20), ("Agda", 10)]
                  [("Haskell", 15), ("Agda", 30)]
    == False
  , planSucceeded [("Java", 30), ("Haskell", 40), ("C", 20)]
                  [("Java", 30), ("Haskell", 20)]
    == True
  ]