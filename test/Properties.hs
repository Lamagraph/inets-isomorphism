{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Properties (properties) where

import qualified Clash.Prelude as C
import Control.Monad (foldM)
import Core.Concrete.ReduceRulesLambda (AgentSimpleLambda (..))
import Core.Node
import Data.Either (isLeft)
import Data.List (permutations)
import qualified Data.Map.Strict as M
import Hedgehog (Gen, Property, assert, forAll, forAllWith, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Isomorphism
import NetSnapshot
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

type Net = NetSnapshot 2 AgentSimpleLambda

type Slot = (Int, Int)

allAddresses :: [AddressNumber]
allAddresses = [minBound .. maxBound]

arity :: AgentSimpleLambda -> Int
arity Erase = 1
arity _ = 3

idOf :: Int -> IdOfPort 2
idOf 0 = Primary
idOf 1 = Id 0
idOf _ = Id 1

showNet :: Net -> String
showNet = show . M.toList . snapNodes

genLinks :: Int -> Gen ([AgentSimpleLambda], [(Slot, Slot)])
genLinks n = do
  first <- Gen.element [Apply, Abstract, Erase]
  (agents, pool, links) <- foldM grow ([first], slotsOf 0 first, []) [1 .. n - 1]
  shuffled <- Gen.shuffle pool
  extra <- pairUp shuffled
  pure (agents, links ++ extra)
 where
  slotsOf k a = [(k, s) | s <- [0 .. arity a - 1]]
  grow (agents, pool, links) k = do
    agent <- Gen.element (if length pool <= 1 then [Apply, Abstract] else [Apply, Abstract, Erase])
    f <- Gen.element pool
    s <- Gen.element [0 .. arity agent - 1]
    let own = [slot | slot@(_, i) <- slotsOf k agent, i /= s]
    pure (agents ++ [agent], filter (/= f) pool ++ own, links ++ [(f, (k, s))])
  pairUp (a : b : rest) = do
    keep <- Gen.bool
    more <- pairUp rest
    pure (if keep then (a, b) : more else more)
  pairUp _ = pure []

assemble :: [AgentSimpleLambda] -> [(Slot, Slot)] -> [AddressNumber] -> Net
assemble agents links addrs =
  NetSnapshot (M.fromList [(addrs !! k, mkNode k a) | (k, a) <- zip [0 ..] agents])
 where
  conn = M.fromList (concatMap (\(x, y) -> [(x, y), (y, x)]) links)
  portAt k s = fmap (\(j, t) -> Port (addrs !! j) (idOf t)) (M.lookup (k, s) conn)
  auxSlot k a s = if a == Erase then Nothing else Just (portAt k s)
  mkNode k a = Node (portAt k 0) (auxSlot k a 1 C.:> auxSlot k a 2 C.:> C.Nil) a

genNet :: Int -> Gen Net
genNet maxNodes = do
  n <- Gen.int (Range.linear 1 maxNodes)
  (agents, links) <- genLinks n
  addrs <- Gen.shuffle allAddresses
  pure (assemble agents links addrs)

relabel :: [AddressNumber] -> AddressNumber -> AddressNumber
relabel shuffled a = M.findWithDefault a a (M.fromList (zip allAddresses shuffled))

flipLabel :: AgentSimpleLambda -> AgentSimpleLambda
flipLabel Apply = Abstract
flipLabel _ = Apply

flipAt :: AddressNumber -> Net -> Net
flipAt a net =
  net{snapNodes = M.adjust (\n -> n{_nodeType = flipLabel (_nodeType n)}) a (snapNodes net)}

bruteForce :: Net -> Net -> Bool
bruteForce net1 net2 =
  M.size (snapNodes net1) == M.size (snapNodes net2)
    && any try (permutations (M.keys (snapNodes net2)))
 where
  keys1 = M.keys (snapNodes net1)
  try p =
    let table = M.fromList (zip keys1 p)
     in snapNodes (mapAddresses (\a -> M.findWithDefault a a table) net1) == snapNodes net2

genPair :: Gen (Net, Net)
genPair = do
  net1 <- genNet 5
  net2 <- Gen.choice [renumbered net1, flipped net1, genNet 5]
  pure (net1, net2)
 where
  renumbered net = (\shuffled -> mapAddresses (relabel shuffled) net) <$> Gen.shuffle allAddresses
  flipped net = (`flipAt` net) <$> Gen.element (M.keys (snapNodes net))

prop_renumbering :: Property
prop_renumbering = property $ do
  net <- forAllWith showNet (genNet 12)
  shuffled <- forAll (Gen.shuffle allAddresses)
  checkIsomorphism net (mapAddresses (relabel shuffled) net) === Right ()

prop_label_change :: Property
prop_label_change = property $ do
  net <- forAllWith showNet (genNet 12)
  address <- forAll (Gen.element (M.keys (snapNodes net)))
  assert (isLeft (checkIsomorphism net (flipAt address net)))

prop_brute_force :: Property
prop_brute_force = property $ do
  (net1, net2) <- forAllWith (\(a, b) -> showNet a ++ "\n" ++ showNet b) genPair
  isIsomorphic net1 net2 === bruteForce net1 net2

properties :: TestTree
properties =
  testGroup
    "random nets"
    [ testPropertyNamed "renumbering keeps the net" "prop_renumbering" prop_renumbering
    , testPropertyNamed "changing an agent breaks the isomorphism" "prop_label_change" prop_label_change
    , testPropertyNamed "agrees with brute force on small nets" "prop_brute_force" prop_brute_force
    ]
