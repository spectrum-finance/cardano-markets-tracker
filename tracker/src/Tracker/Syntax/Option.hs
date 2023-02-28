module Tracker.Syntax.Option
  ( unNone
  ) where

unNone :: [Maybe a] -> [a]
unNone = foldr (\x acc ->
    case x of
      Just x1 -> x1:acc
      _       -> acc
    ) []