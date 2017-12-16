module Hangman

import StdEnv, StdLib

:: State :== ([Char], Int, [Char])

makeState :: String -> State
makeState str = (fromString str,5,[])

gameOver :: State -> Bool
gameOver (_, life,_) = life == 0

showState :: State -> String
showState (word, _, tries) = toString [ resolveChar w \\ w <- word ]
  where
    resolveChar c
      | isMember c tries = c
      | otherwise = '_'

guessLetter :: Char State -> State
guessLetter c (word, life, tries)
  | isMember c tries = (word, life, tries)
  | isMember c word = (word, life, [c:tries])
  | otherwise = (word, life-1, [c:tries])

play :: [Char] State -> State
play [] s = s
play [c:cs] s
  | gameOver s = s
  | otherwise = play cs (guessLetter c s)

ending :: State -> Maybe Int
ending (word, life, tries)
  | life > 0 && (and [ isMember w tries \\ w <- word ]) = Just (length tries)
  | otherwise = Nothing

Start = (and result,zip2 [1..] (result), tests)
  where
    result = map and tests

tests = [
          test_makeState
         ,test_gameOver
         ,test_showState
         ,test_guessLetter
         ,test_play
         ,test_ending
        ]

test_makeState =
  [ makeState ""     == ([], 5, [])
  , makeState "alma" == (['a','l','m','a'], 5, [])
  ]

test_gameOver =
  [      gameOver ([], 0, [])
  , not (gameOver ([], 1, []))
  ,      gameOver (fromString "alma", 0, ['a'])
  , not (gameOver (fromString "korte", 1, ['o','k']))
  ]

test_showState =
  [ showState ([], 2, [])                        == ""
  , showState (['a'], 2, [])                     == "_"
  , showState (['a'], 2, ['a'])                  == "a"
  , showState (fromString "abcba", 2, ['c','a']) == "a_c_a"
  , showState (fromString "alma", 2, ['a'])      == "a__a"
  ]

test_guessLetter =
  [ guessLetter 'a' (fromString "alma", 1, ['b','a','c'])
    == (fromString "alma", 1, ['b','a','c'])
  , guessLetter 'b' (fromString "alma", 1, ['a'])
    == (fromString "alma", 0, ['b','a'])
  , guessLetter 'a' (fromString "alma", 1, [])
    == (fromString "alma", 1, ['a'])
  ]

test_play =
  [ play [] (fromString "alma", 2, ['b','a'])
    == (fromString "alma", 2, ['b','a'])
  , play ['n'] (fromString "alma", 0, ['a','l'])
    == (fromString "alma", 0, ['a','l'])
  , play ['a'..'c'] (fromString "acd", 4, ['d'])
    == (fromString "acd", 3, ['c','b','a','d'])
  , play ['e'..'h'] (fromString "acd", 4, ['d'])
    == (fromString "acd", 0, ['h','g','f','e','d'])
  , play ['e'..'i'] (fromString "acd", 4, ['d'])
    == (fromString "acd", 0, ['h','g','f','e','d'])
  ]

test_ending =
  [ ending (fromString "alma", 0, ['m','a','l'])         == Nothing
  , ending (fromString "alma", 1, ['m','a'])             == Nothing
  , ending (fromString "alma", 1, ['m','a','l'])         == Just 3
  , ending (fromString "alma", 1, ['m','a','l','b','c']) == Just 5
  ]