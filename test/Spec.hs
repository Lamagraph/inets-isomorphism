{-# LANGUAGE DataKinds #-}

module Main (main) where

import Assertions
import qualified Clash.Prelude as C
import Core.Concrete.Initial (
  initialEpsAppToIdNode,
  initialEpsAppToIdSimpleNode,
  initialIdApplyToIdNode,
  initialIdEraseNode,
  initialLoopEdge,
 )
import Core.Concrete.ReduceRulesLambda (AgentSimpleLambda (..))
import Core.Node
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Isomorphism
import NetSnapshot
import Properties (properties)
import Test.Tasty
import Test.Tasty.HUnit

type Net = NetSnapshot 2 AgentSimpleLambda

samples :: [(String, Net)]
samples =
  [ ("id apply id", fromVec initialIdApplyToIdNode)
  , ("eps app id", fromVec initialEpsAppToIdNode)
  , ("eps app id simple", fromVec initialEpsAppToIdSimpleNode)
  , ("id erase", fromVec initialIdEraseNode)
  , ("loop edge", fromVec (fmap (fmap _containedNode) initialLoopEdge))
  ]

relabelings :: [(String, AddressNumber -> AddressNumber)]
relabelings =
  [ ("identity", id)
  , ("shift", (+ 5))
  , ("reflect", (15 -))
  , ("multiply", \a -> 3 * a + 1)
  ]

fork :: Bool -> Net
fork eraseFirst = fromVec cells
 where
  eraseSlot = if eraseFirst then 0 else 1
  absSlot = if eraseFirst then 1 else 0
  toErase = Just (Connected (Port 1 Primary))
  toAbs = Just (Connected (Port 2 Primary))
  aux = if eraseFirst then toErase C.:> toAbs C.:> C.Nil else toAbs C.:> toErase C.:> C.Nil
  apply = Node NotConnected aux Apply
  erase = Node (Connected (Port 0 (Id eraseSlot))) (C.repeat Nothing) Erase
  abstr = Node (Connected (Port 0 (Id absSlot))) (C.repeat (Just NotConnected)) Abstract
  cells :: C.Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
  cells = Just apply C.+>> Just erase C.+>> Just abstr C.+>> C.repeat Nothing

dangling :: Bool -> Net
dangling erasePort0 = fromVec cells
 where
  slot = if erasePort0 then 0 else 1
  toErase = Just (Connected (Port 1 Primary))
  free = Just NotConnected
  aux = if erasePort0 then toErase C.:> free C.:> C.Nil else free C.:> toErase C.:> C.Nil
  top = Node NotConnected aux Abstract
  erase = Node (Connected (Port 0 (Id slot))) (C.repeat Nothing) Erase
  cells :: C.Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
  cells = Just top C.+>> Just erase C.+>> C.repeat Nothing

renumbering :: TestTree
renumbering =
  testGroup
    "renumbering keeps the net"
    [ testCase (name ++ ", " ++ label) (assertIsomorphic net (mapAddresses f net))
    | (name, net) <- samples
    , (label, f) <- relabelings
    ]

rejections :: TestTree
rejections =
  testGroup
    "different nets are rejected"
    [ testCase "different agent label" $
        checkIsomorphism base relabelled @?= Left (LabelMismatch 1 1)
    , testCase "unreachable node" $
        checkIsomorphism base withJunk @?= Left (Disconnected 3 4 3)
    ]
 where
  base = fromVec initialIdApplyToIdNode
  relabelled = base{snapNodes = M.adjust (\n -> n{_nodeType = Erase}) 1 (snapNodes base)}
  withJunk = base{snapNodes = M.insert 7 (snapNodes base M.! 1) (snapNodes base)}

portOrder :: TestTree
portOrder =
  testGroup
    "port positions matter"
    [ testCase "same net with other addresses" $
        assertIsomorphic (fork True) (mapAddresses (+ 3) (fork True))
    , testCase "swapped auxiliary ports of apply" $
        assertBool "expected a mismatch" (isLeft (checkIsomorphism (fork True) (fork False)))
    , testCase "free port on another position" $
        checkIsomorphism (dangling True) (dangling False) @?= Left (PortMismatch 0 0 1)
    ]

openApply :: Bool -> Net
openApply swapped = fromVec cells
 where
  aux =
    if swapped
      then Just NotConnected C.:> Just (Connected (Port 1 Primary)) C.:> C.Nil
      else Just (Connected (Port 1 Primary)) C.:> Just NotConnected C.:> C.Nil
  slot = if swapped then 1 else 0
  apply = Node NotConnected aux Apply
  erase = Node (Connected (Port 0 (Id slot))) (C.repeat Nothing) Erase
  cells :: C.Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
  cells = Just apply C.+>> Just erase C.+>> C.repeat Nothing

subnets :: TestTree
subnets =
  testGroup
    "open nets keep the positions of free ports"
    [ testCase "same free ports, other addresses" $
        assertIsomorphic (openApply False) (mapAddresses (+ 4) (openApply False))
    , testCase "free port moved to another position" $
        checkIsomorphism (openApply False) (openApply True) @?= Left (PortMismatch 0 0 1)
    , testCase "checkFrom starts from an interface node" $
        checkFrom [(0, 4)] (openApply False) (mapAddresses (+ 4) (openApply False)) @?= Right ()
    ]

twoAbstracts :: Net
twoAbstracts = fromVec cells
 where
  toB2 = Just (Connected (Port 1 (Id 1)))
  toB1 = Just (Connected (Port 0 (Id 1)))
  toErase = Just (Connected (Port 2 Primary))
  b1 = Node NotConnected (Just NotConnected C.:> toB2 C.:> C.Nil) Abstract
  b2 = Node NotConnected (toErase C.:> toB1 C.:> C.Nil) Abstract
  erase = Node (Connected (Port 1 (Id 0))) (C.repeat Nothing) Erase
  cells :: C.Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
  cells = Just b1 C.+>> Just b2 C.+>> Just erase C.+>> C.repeat Nothing

swapFirstTwo :: AddressNumber -> AddressNumber
swapFirstTwo a
  | a == 0 = 1
  | a == 1 = 0
  | otherwise = a

emptyNet :: Net
emptyNet = NetSnapshot M.empty

startChoice :: TestTree
startChoice =
  testGroup
    "start of the traversal"
    [ testCase "the second candidate is the right one" $
        assertIsomorphic (mapAddresses swapFirstTwo twoAbstracts) twoAbstracts
    , testCase "no candidate fits" $
        assertBool "expected a mismatch" (isLeft (checkIsomorphism twoAbstracts otherLabel))
    , testCase "empty net against a net" $
        checkIsomorphism emptyNet twoAbstracts @?= Left NoStartNode
    , testCase "net against an empty net" $
        checkIsomorphism twoAbstracts emptyNet @?= Left NoStartNode
    , testCase "two empty nets" $
        checkIsomorphism emptyNet emptyNet @?= Right ()
    ]
 where
  otherLabel = twoAbstracts{snapNodes = M.adjust (\n -> n{_nodeType = Abstract}) 2 (snapNodes twoAbstracts)}

main :: IO ()
main = defaultMain (testGroup "Isomorphism" [renumbering, rejections, portOrder, subnets, startChoice, properties])
