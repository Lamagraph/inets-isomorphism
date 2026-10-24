{-# LANGUAGE DeriveGeneric #-}

module Parser 
    ( parseGraphFromFile 
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decodeFileStrict)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Graph.Inductive (Gr, mkGraph, LNode, LEdge)


data JgfMetadata = JgfMetadata { label :: String } deriving (Show, Generic)
data JgfNode = JgfNode { metadata :: JgfMetadata } deriving (Show, Generic)
data JgfEdge = JgfEdge { source :: String, target :: String, relation :: String } deriving (Show, Generic)
data JgfGraph = JgfGraph { nodes :: Map String JgfNode, edges :: [JgfEdge] } deriving (Show, Generic)
data JgfRoot = JgfRoot { graph :: JgfGraph } deriving (Show, Generic)

instance FromJSON JgfMetadata
instance FromJSON JgfNode
instance FromJSON JgfEdge
instance FromJSON JgfGraph
instance FromJSON JgfRoot


convertJgfToFgl :: JgfGraph -> Gr String String
convertJgfToFgl jgf = mkGraph fglNodes fglEdges
  where
    strIds = M.keys (nodes jgf)
    dict = M.fromList (zip strIds [1..])

    fglNodes :: [LNode String]
    fglNodes = map convertNode (M.toList (nodes jgf))
      where
        convertNode (strId, jgfNode) = 
            let intId = dict M.! strId
                lbl = label (metadata jgfNode)
            in (intId, lbl)

    fglEdges :: [LEdge String]
    fglEdges = map convertEdge (edges jgf)
      where
        convertEdge jgfEdge =
            let srcId = dict M.! source jgfEdge
                tgtId = dict M.! target jgfEdge
                rel = relation jgfEdge
            in (srcId, tgtId, rel)


parseGraphFromFile :: FilePath -> IO (Maybe (Gr String String))
parseGraphFromFile path = do
    maybeRoot <- decodeFileStrict path :: IO (Maybe JgfRoot)
    
    case maybeRoot of
        Nothing   -> return Nothing
        Just root -> return (Just (convertJgfToFgl (graph root)))