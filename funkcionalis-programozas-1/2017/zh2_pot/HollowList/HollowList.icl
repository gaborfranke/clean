module HollowList

import StdEnv, StdLib, GenEq

:: HollowList a = Empty
  | Hole (HollowList a)
  | Cons a (HollowList a)

derive gEq HollowList, Maybe

empty :: HollowList a
empty = Empty

fromList :: [a] -> HollowList a
fromList [] = Empty
fromList [x:xs] = Cons x (fromList xs)

hollowFilter :: (a -> Bool) (HollowList a) -> HollowList a
hollowFilter fv Empty = Empty
hollowFilter fv (Hole list) = Hole (hollowFilter fv list)
hollowFilter fv (Cons value list)
  | fv value = Cons value (hollowFilter fv list)
  | otherwise = Hole (hollowFilter fv list)

class Shrinkable t where
  shrink :: t -> Maybe t

instance Shrinkable (HollowList a) where
  shrink list
    | hasHole list = Just (removeFirstHole list)
    | otherwise = Nothing

hasHole :: (HollowList a) -> Bool
hasHole Empty = False
hasHole (Hole list) = True
hasHole (Cons _ list) = hasHole list

removeFirstHole :: (HollowList a) -> (HollowList a)
removeFirstHole (Hole list) = list
removeFirstHole (Cons value list) = Cons value (removeFirstHole list)

compact :: t -> t | Shrinkable t
compact t = compactH (shrink t) t
  where
    compactH Nothing t = t
    compactH (Just list) _ = compactH (shrink list) list

instance toString (HollowList a) | toString a where
  toString Empty = ""
  toString (Hole Empty) = "_"
  toString (Hole list) = "_, " +++ (toString list)
  toString (Cons value Empty) = toString value
  toString (Cons value list) = (toString value) +++ ", " +++ (toString list)

diff :: (HollowList a) -> String | toString a
diff list = "[" +++ (toString list) +++ "] -> [" +++ (toString (compact list)) +++ "]"

Start = (and result,zip2 [1..] (result), tests)
  where
    result = map and tests

tests = [ hollowList_test
  , fromList_test
  , hollowFilter_test
  , shrink_test
  , compact_test
  , toString_test
  , diff_test
  ]
  
hollowList_test =
  [ e                            === e
  , Hole e                       === Hole e
  , Cons 1 (Cons 2 Empty)        === Cons 1 (Cons 2 Empty)
  , Cons 1 (Hole (Cons 2 Empty)) =!= Cons 1 (Cons 2 Empty)
  ]
  where
    e :: HollowList Int
    e = empty

fromList_test =
  [ fromList []     === e
  , fromList [1]    === Cons 1 Empty
  , fromList [1..3] === Cons 1 (Cons 2 (Cons 3 Empty))
  ]
  where
    e :: HollowList Int
    e = empty

hollowFilter_test =
  [ hollowFilter (\n -> n > 0) empty          === Empty
  , hollowFilter (\n -> n > 0) (Hole Empty)   === Hole Empty
  , hollowFilter (\n -> n > 0) (Cons 1 Empty) === Cons 1 Empty
  , hollowFilter (\n -> n > 0) (Cons 0 Empty) === Hole Empty
  , hollowFilter (\n -> n > 0)
                 (Cons 1 (Cons (~1) (Hole (Cons (~2) (Cons 2 Empty)))))
    === (Cons 1 (Hole (Hole (Hole (Cons 2 Empty)))))
  ]

shrink_test =
  [ shrink e                                === Nothing
  , shrink (Cons 1 Empty)                   === Nothing
  , shrink (Cons 1 (Cons 2 (Cons 3 Empty))) === Nothing
  , shrink (Hole e)                         === Just e
  , shrink (Hole (Hole e))                  === Just (Hole e)
  , shrink (Cons 1 (Hole (Cons 2 (Hole Empty))))
    === Just (Cons 1 (Cons 2 (Hole Empty)))
  ]
  where
    e :: HollowList Int
    e = empty

compact_test =
  [ compact e                                     === e
  , compact (Cons 1 Empty)                        === Cons 1 Empty
  , compact (Hole e)                              === e
  , compact (Hole (Hole e))                       === e
  , compact (Cons 1 (Hole (Cons 2 (Hole Empty)))) === Cons 1 (Cons 2 (Empty))
  , compact (Cons 1 (Cons 2 (Cons 3 Empty)))
    === (Cons 1 (Cons 2 (Cons 3 Empty)))
  ]
  where
    e :: HollowList Int
    e = empty

toString_test =
  [ toString e                                     == ""
  , toString (Hole e)                              == "_"
  , toString (Hole (Hole e))                       == "_, _"
  , toString (Cons 1 Empty)                        == "1"
  , toString (Cons 1 (Cons 2 Empty))               == "1, 2"
  , toString (Cons 1 (Hole (Cons 2 (Hole Empty)))) == "1, _, 2, _"
  ]
  where
    e :: HollowList Int
    e = empty

diff_test =
  [ diff e                                     == "[] -> []"
  , diff (Cons 1 Empty)                        == "[1] -> [1]"
  , diff (Cons 1 (Cons 2 (Cons 3 Empty)))      == "[1, 2, 3] -> [1, 2, 3]"
  , diff (Hole e)                              == "[_] -> []"
  , diff (Hole (Hole e))                       == "[_, _] -> []"
  , diff (Cons 1 (Hole (Cons 2 (Hole Empty)))) == "[1, _, 2, _] -> [1, 2]"
  ]
  where
    e :: HollowList Int
    e = empty