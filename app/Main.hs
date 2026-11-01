module Main (main) where

import Core.Concrete.Initial
  ( initialEpsAppToIdNode
  , initialEpsAppToIdSimpleNode
  , initialIdApplyToIdNode
  )
import Isomorphism (checkIsomorphism)
import NetSnapshot (fromVec, mapAddresses)
import Reciprocity (brokenLinks)

main :: IO ()
main = do
  let net = fromVec 0 initialIdApplyToIdNode
      moved = mapAddresses (+ 5) net
      other = fromVec 1 initialEpsAppToIdSimpleNode
  print (checkIsomorphism net net)
  print (checkIsomorphism net moved)
  print (checkIsomorphism net other)
  print (brokenLinks (fromVec 1 initialEpsAppToIdNode))