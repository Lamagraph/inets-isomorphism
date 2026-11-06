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
  let net = fromVec initialIdApplyToIdNode
      moved = mapAddresses (+ 5) net
      other = fromVec initialEpsAppToIdSimpleNode
  print (checkIsomorphism net net)
  print (checkIsomorphism net moved)
  print (checkIsomorphism net other)
  print (brokenLinks (fromVec initialEpsAppToIdNode))