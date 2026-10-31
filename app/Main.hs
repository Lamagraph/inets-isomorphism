module Main (main) where

import Core.Concrete.Initial (initialEpsAppToIdSimpleNode, initialIdApplyToIdNode)
import Isomorphism (checkIsomorphism)
import NetSnapshot (fromVec, mapAddresses)

main :: IO ()
main = do
  let net = fromVec 0 initialIdApplyToIdNode
      moved = mapAddresses (+ 5) net
      other = fromVec 1 initialEpsAppToIdSimpleNode
  print (checkIsomorphism net net)
  print (checkIsomorphism net moved)
  print (checkIsomorphism net other)