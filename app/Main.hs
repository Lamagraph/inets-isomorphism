module Main (main) where

import Core.Concrete.Initial
import Core.Node

main :: IO ()
main = print (foldr countNode (0 :: Int) initialIdApplyToIdNode)
  where
    countNode maybeNode count =
      case maybeNode of
        Just _ -> count + 1
        Nothing -> count
