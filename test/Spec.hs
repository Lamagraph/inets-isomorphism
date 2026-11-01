{-# LANGUAGE DataKinds #-}

module Main (main) where

import qualified Clash.Prelude as C
import Core.Concrete.Initial
  ( initialEpsAppToIdNode
  , initialEpsAppToIdSimpleNode
  , initialIdApplyToIdNode
  , initialIdEraseNode
  , initialLoopEdge
  )
import Core.Concrete.ReduceRulesLambda (AgentSimpleLambda (..))
import Core.Node
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Isomorphism
import NetSnapshot
import Test.Tasty
import Test.Tasty.HUnit
import Reciprocity
import Assertions

type Net = NetSnapshot 2 AgentSimpleLambda

samples :: [(String, Net)]
samples =
  [ ("id apply id", fromVec 0 initialIdApplyToIdNode)
  , ("eps app id", fromVec 1 initialEpsAppToIdNode)
  , ("eps app id simple", fromVec 1 initialEpsAppToIdSimpleNode)
  , ("id erase", fromVec 0 initialIdEraseNode)
  , ("loop edge", fromVec 0 (fmap (fmap _containedNode) initialLoopEdge))
  ]

relabelings :: [(String, AddressNumber -> AddressNumber)]
relabelings =
  [ ("identity", id)
  , ("shift", (+ 5))
  , ("reflect", (15 -))
  , ("multiply", \a -> 3 * a + 1)
  ]

fork :: Bool -> Net
fork eraseFirst = fromVec 0 cells
 where
  eraseSlot = if eraseFirst then 0 else 1
  absSlot = if eraseFirst then 1 else 0
  toErase = Just (Connected (Port 1 Primary))
  toAbs = Just (Connected (Port 2 Primary))
  aux = if eraseFirst then toErase C.:> toAbs C.:> C.Nil else toAbs C.:> toErase C.:> C.Nil
  apply = Node NotConnected aux Apply
  erase = Node (Connected (Port 0 (Id eraseSlot))) (C.repeat Nothing) Erase
  abstr = Node (Connected (Port 0 (Id absSlot))) (C.repeat (Just NotConnected)) Abstract
  cells :: C.Vec 16 (Maybe (Node 2 AgentSimpleLambda))
  cells = Just apply C.+>> Just erase C.+>> Just abstr C.+>> C.repeat Nothing

dangling :: Bool -> Net
dangling erasePort0 = fromVec 0 cells
 where
  slot = if erasePort0 then 0 else 1
  toErase = Just (Connected (Port 1 Primary))
  free = Just NotConnected
  aux = if erasePort0 then toErase C.:> free C.:> C.Nil else free C.:> toErase C.:> C.Nil
  root = Node NotConnected aux Abstract
  erase = Node (Connected (Port 0 (Id slot))) (C.repeat Nothing) Erase
  cells :: C.Vec 16 (Maybe (Node 2 AgentSimpleLambda))
  cells = Just root C.+>> Just erase C.+>> C.repeat Nothing

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
  base = fromVec 0 initialIdApplyToIdNode
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

idApplyId :: Net
idApplyId = fromVec 0 initialIdApplyToIdNode

rewired :: Net
rewired =
  idApplyId
    { snapNodes =
        M.adjust (\n -> n{_primaryPort = Just (Port 0 (Id 0))}) 1 (snapNodes idApplyId)
    }

reciprocity :: TestTree
reciprocity =
  testGroup
    "pointers are reciprocal"
    ( [ testCase name (brokenLinks net @?= [])
      | (name, net) <- samples
      , name /= "eps app id"
      ]
        ++ [testCase "one pointer rewired" (brokenLinks rewired @?= [Broken 0 2, Broken 1 0])]
    )

main :: IO ()
main = defaultMain (testGroup "Isomorphism" [renumbering, rejections, portOrder, reciprocity])