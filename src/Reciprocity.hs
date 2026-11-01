module Reciprocity
  ( Broken (..)
  , brokenLinks
  ) where

import Clash.Prelude (KnownNat)
import Core.Node (AddressNumber, IdOfPort (..), Port (..))
import qualified Data.Map.Strict as M
import NetSnapshot (NetSnapshot (..), slots)

data Broken = Broken AddressNumber Int
  deriving (Eq, Show)

portIndex :: (KnownNat p) => IdOfPort p -> Int
portIndex Primary = 0
portIndex (Id k) = 1 + fromIntegral k

brokenLinks :: (KnownNat p) => NetSnapshot p a -> [Broken]
brokenLinks net =
  [ Broken a i
  | (a, node) <- M.toList (snapNodes net)
  , (i, Just (Just (Port b pb))) <- zip [0 ..] (slots node)
  , not (reciprocated a i b pb)
  ]
 where
  reciprocated a i b pb =
    case M.lookup b (snapNodes net) of
      Nothing -> False
      Just nb -> case drop (portIndex pb) (slots nb) of
        Just (Just (Port a' pa')) : _ -> a' == a && portIndex pa' == i
        _ -> False