module NetSnapshot
  ( NetSnapshot (..)
  , fromVec
  , mapAddresses
  , slots
  ) where

import Clash.Prelude (KnownNat, Vec)
import Core.Node (AddressNumber, Node (..), Port (..))
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

newtype NetSnapshot p a = NetSnapshot
  { snapNodes :: Map AddressNumber (Node p a)
  }

fromVec :: (KnownNat n) => Vec n (Maybe (Node p a)) -> NetSnapshot p a
fromVec cells =
  NetSnapshot
    ( M.fromList
        [ (fromIntegral i, node)
        | (i, Just node) <- zip [0 :: Int ..] (toList cells)
        ]
    )

mapAddresses :: (AddressNumber -> AddressNumber) -> NetSnapshot p a -> NetSnapshot p a
mapAddresses f (NetSnapshot nodes) =
  NetSnapshot (M.fromList [(f k, fixNode n) | (k, n) <- M.toList nodes])
 where
  fixPort port = port{_nodeAddress = f (_nodeAddress port)}
  fixNode n =
    n
      { _primaryPort = fmap fixPort (_primaryPort n)
      , _secondaryPorts = fmap (fmap (fmap fixPort)) (_secondaryPorts n)
      }

slots :: (KnownNat p) => Node p a -> [Maybe (Maybe (Port p))]
slots n = Just (_primaryPort n) : toList (_secondaryPorts n)