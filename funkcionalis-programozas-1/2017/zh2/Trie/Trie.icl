module Trie

import StdEnv, StdLib, GenEq

derive gEq Trie, Maybe

:: Trie v = T (Maybe v) [(Char, Trie v)]

emptyTrie :: Trie v
emptyTrie = T Nothing []

insertTrie :: String v (Trie v) -> (Trie v)
insertTrie str val trie = insertTrieH (toChars str) val trie
  where
    insertTrieH [] val (T _ t) = T (Just val) t
	insertTrieH [x:xs] val (T maybe []) = T maybe [(x, insertTrieH xs val emptyTrie)]
	insertTrieH [x:xs] val (T maybe [(v, t):ys])
      | v == x = T maybe [(x, insertTrieH xs val t):ys]
      | x < v  = T maybe [(x, insertTrieH xs val t),(v, t):ys]
      | otherwise = T maybe ([(v, t)] ++ [(x, insertTrieH xs val (T maybe ys))])

buildTrie :: [(String, v)] -> Trie v
buildTrie xs = foldl buildH emptyTrie xs
  where
    buildH tree (str, v) = insertTrie str v tree

lookupTrie :: String (Trie v) -> Maybe v
lookupTrie str (T m ts) = lookupTrieH (toChars str) (T m ts)
  where
    lookupTrieH [] (T m _) = m
    lookupTrieH [c:cs] (T m ([])) = Nothing
    lookupTrieH [c:cs] (T m ([t:ts]))
      | c == fst t = lookupTrieH cs (snd t)
      | otherwise = lookupTrieH [c:cs] (T m ts)

class Functor f where
  fmap :: (a -> b) (f a) -> (f b)

instance Functor Trie where
  fmap f (T Nothing ts) = T Nothing [ (c, fmap f t) \\ (c,t) <- ts]
  fmap f (T (Just m) ts) = T (Just (f m)) [ (c, fmap f t) \\ (c,t) <- ts]


toChars :: String -> [Char]
toChars srt = [ c \\ c <-: srt]


Start = (and result,zip2 [1..] (result), tests)
  where
    result = map and tests
tests =
      [ test_insertTrie
      , test_buildTrie
      , test_lookupTrie
      , test_fmap
      ]
      
test_insertTrie :: [Bool]
test_insertTrie =
    [ insertTrie ""    0 emptyTrie
	=== T (Just 0) []
    , insertTrie "a"   1 emptyTrie
	=== T Nothing [('a', T (Just 1) [])]

    , insertTrie "b"   2 (insertTrie "a"   1 emptyTrie)
	=== T Nothing [('a', T (Just 1) []), ('b', T (Just 2) [])]
    , insertTrie "a"   1 (insertTrie "b"   2 emptyTrie)
	=== T Nothing [('a', T (Just 1) []), ('b', T (Just 2) [])]

    , insertTrie "ab"  3 (insertTrie "a"   1 emptyTrie)
	=== T Nothing [('a', T (Just 1) [('b', T (Just 3) [])])]
    , insertTrie "abc" 4 emptyTrie
	=== T Nothing [('a', T Nothing [('b', T Nothing [('c', T (Just 4) [])])])]
    ]
    

test_buildTrie :: [Bool]
test_buildTrie =
    [ buildTrie [] === emptyTrieInt
    , buildTrie [("", 0)] === T (Just 0) []
    , buildTrie [("", 0), ("ab", 1), ("abc", 2), ("abd", 3)] ===
	T (Just 0) [('a', T Nothing [('b', T (Just 1) [('c', T (Just 2) []), ('d', T (Just 3) [])])])]
    ]
    where
	emptyTrieInt :: Trie Int
	emptyTrieInt = emptyTrie
	

test_lookupTrie =
    [ lookupTrie "hello" emptyTrieBool == Nothing
    , lookupTrie ""    t == Just 0
    , lookupTrie "a"   t == Nothing
    , lookupTrie "ab"  t == Just 1
    , lookupTrie "abc" t == Just 2
    , lookupTrie "abd" t == Just 3
    , lookupTrie "abe" t == Nothing
    ]
    where
	emptyTrieBool :: Trie Bool
	emptyTrieBool = emptyTrie
	t :: Trie Int
	t = T (Just 0) [('a', T Nothing [('b', T (Just 1) [('c', T (Just 2) []), ('d', T (Just 3) [])])])]



test_fmap :: [Bool]
test_fmap =
    [ fmap ((+) 1) emptyTrie === emptyTrie
    , fmap ((+) 1) (T (Just 0) []) === (T (Just 1) [])
    , fmap ((+) 1) (T (Just 0) [('a', T Nothing [('b', T (Just 1) [('c', T (Just 2) []), ('d', T (Just 3) [])])])])
            === T (Just 1) [('a', T Nothing [('b', T (Just 2) [('c', T (Just 3) []), ('d', T (Just 4) [])])])]
    , fmap (\x -> if (x rem 2 == 0) "even" "odd") (T (Just 0) [('a', T Nothing [('b', T (Just 1) [('c', T (Just 2) []), ('d', T (Just 3) [])])])])
            === (T (Just "even") [('a', T Nothing [('b', T (Just "odd") [('c', T (Just "even") []), ('d', T (Just "odd") [])])])])
    ]
    
    
