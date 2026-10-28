module Main (main) where

import Core.Concrete.Initial
  ( initialEpsAppToIdNode
  , initialEpsAppToIdSimpleNode
  , initialIdApplyToIdNode
  )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main =
  defaultMain $
    testGroup
      "Initial interaction nets"
      [ testCase "initialIdApplyToIdNode contains 3 nodes" $
          countOccupied initialIdApplyToIdNode @?= 3
      , testCase "initialEpsAppToIdNode contains 5 nodes" $
          countOccupied initialEpsAppToIdNode @?= 5
      , testCase "initialEpsAppToIdSimpleNode contains 3 nodes" $
          countOccupied initialEpsAppToIdSimpleNode @?= 3
      ]

countOccupied :: Foldable t => t (Maybe a) -> Int
countOccupied = foldr countOne 0
  where
    countOne maybeNode total =
      case maybeNode of
        Just _ -> total + 1
        Nothing -> total
