module Assertions
  ( assertIsomorphic
  ) where

import Clash.Prelude (KnownNat)
import Isomorphism (Mismatch (..), Side (..), checkIsomorphism)
import NetSnapshot (NetSnapshot)
import Test.Tasty.HUnit (Assertion, assertFailure)

assertIsomorphic :: (KnownNat p, Eq a) => NetSnapshot p a -> NetSnapshot p a -> Assertion
assertIsomorphic expected actual =
  case checkIsomorphism expected actual of
    Right () -> pure ()
    Left mismatch -> assertFailure ("nets are not isomorphic: " ++ describe mismatch)

sideName :: Side -> String
sideName FirstNet = "expected"
sideName SecondNet = "actual"

describe :: Mismatch -> String
describe (MissingNode side addr) =
  "the " ++ sideName side ++ " net has no node at address " ++ show addr
describe (LabelMismatch a b) =
  "agents differ: address "
    ++ show a
    ++ " in the expected net, address "
    ++ show b
    ++ " in the actual net"
describe (PortMismatch a b i) =
  "port "
    ++ show i
    ++ " differs between node "
    ++ show a
    ++ " (expected net) and node "
    ++ show b
    ++ " (actual net); port 0 is the primary port"
describe (MappingConflict a b) =
  "node "
    ++ show a
    ++ " (expected net) and node "
    ++ show b
    ++ " (actual net) cannot be matched: one of them is already matched with another node"
describe (Disconnected n1 n2 matched) =
  "not every node is reachable from the root: the expected net has "
    ++ show n1
    ++ " nodes, the actual net has "
    ++ show n2
    ++ ", matched "
    ++ show matched