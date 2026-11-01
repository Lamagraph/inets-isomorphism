module Isomorphism
  ( Mismatch (..)
  , Side (..)
  , checkIsomorphism
  , checkFrom
  , isIsomorphic
  ) where

import Clash.Prelude (KnownNat)
import Control.Monad (foldM, unless)
import Core.Node (AddressNumber, Node (..), Port (..))
import qualified Data.Map.Strict as M
import NetSnapshot (NetSnapshot (..), slots)

data Side = FirstNet | SecondNet
  deriving (Eq, Show)

data Mismatch
  = MissingNode Side AddressNumber
  | LabelMismatch AddressNumber AddressNumber
  | PortMismatch AddressNumber AddressNumber Int
  | MappingConflict AddressNumber AddressNumber
  | Disconnected Int Int Int
  deriving (Eq, Show)

data Matching = Matching
  { fwd :: M.Map AddressNumber AddressNumber
  , bwd :: M.Map AddressNumber AddressNumber
  }

bind :: Matching -> AddressNumber -> AddressNumber -> Either Mismatch (Maybe Matching)
bind m a b =
  case (M.lookup a (fwd m), M.lookup b (bwd m)) of
    (Nothing, Nothing) -> Right (Just (Matching (M.insert a b (fwd m)) (M.insert b a (bwd m))))
    (Just b', Just a') | b' == b && a' == a -> Right Nothing
    _ -> Left (MappingConflict a b)

portPairs
  :: (KnownNat p)
  => AddressNumber -> AddressNumber -> Node p a -> Node p a
  -> Either Mismatch [(AddressNumber, AddressNumber)]
portPairs a b na nb = concat <$> sequence (zipWith3 one [0 :: Int ..] (slots na) (slots nb))
 where
  one i sa sb = case (sa, sb) of
    (Nothing, Nothing) -> Right []
    (Just Nothing, Just Nothing) -> Right []
    (Just (Just (Port x px)), Just (Just (Port y py)))
      | px == py -> Right [(x, y)]
    _ -> Left (PortMismatch a b i)

checkFrom
  :: (KnownNat p, Eq a)
  => [(AddressNumber, AddressNumber)]
  -> NetSnapshot p a
  -> NetSnapshot p a
  -> Either Mismatch ()
checkFrom starts net1 net2 = do
  (m0, queue0) <- foldM bindNew (Matching M.empty M.empty, []) starts
  m <- go m0 queue0
  let n1 = M.size (snapNodes net1)
      n2 = M.size (snapNodes net2)
      matched = M.size (fwd m)
  unless (n1 == matched && n2 == matched) (Left (Disconnected n1 n2 matched))
 where
  bindNew (m, queue) (a, b) = do
    r <- bind m a b
    pure $ case r of
      Nothing -> (m, queue)
      Just m' -> (m', (a, b) : queue)

  look side net addr =
    maybe (Left (MissingNode side addr)) Right (M.lookup addr (snapNodes net))

  go m [] = Right m
  go m ((a, b) : rest) = do
    na <- look FirstNet net1 a
    nb <- look SecondNet net2 b
    unless (_nodeType na == _nodeType nb) (Left (LabelMismatch a b))
    pairs <- portPairs a b na nb
    (m', new) <- foldM bindNew (m, []) pairs
    go m' (new ++ rest)

checkIsomorphism
  :: (KnownNat p, Eq a)
  => NetSnapshot p a -> NetSnapshot p a -> Either Mismatch ()
checkIsomorphism net1 net2 = checkFrom [(snapRoot net1, snapRoot net2)] net1 net2

isIsomorphic :: (KnownNat p, Eq a) => NetSnapshot p a -> NetSnapshot p a -> Bool
isIsomorphic net1 net2 = either (const False) (const True) (checkIsomorphism net1 net2)
