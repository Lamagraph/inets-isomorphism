module Invariants (getLabelsInventory, getEdgeLabelsInventory, getDegreeSequence, checkInvariants, printNodes) where

import Data.Graph.Inductive.Graph (noNodes, size, labNodes, labEdges, nodes, indeg, outdeg, LNode)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (sort)

getLabelsInventory :: Ord a => Gr a b -> [a]
getLabelsInventory graph = sort $ map snd (labNodes graph)

getEdgeLabelsInventory :: Ord b => Gr a b -> [b]
getEdgeLabelsInventory graph = sort $ map (\(_, _, label) -> label) (labEdges graph)

getDegreeSequence :: Gr a b -> [(Int, Int)]
getDegreeSequence graph = sort $ map (\v -> (indeg graph v, outdeg graph v)) (nodes graph)

checkInvariants :: (Ord a, Ord b) => Gr a b -> Gr a b -> Bool
checkInvariants g1 g2 =
    let 
        v1 = noNodes g1
        v2 = noNodes g2
        e1 = size g1
        e2 = size g2
        nodeInv1 = getLabelsInventory g1
        nodeInv2 = getLabelsInventory g2
        edgeInv1 = getEdgeLabelsInventory g1
        edgeInv2 = getEdgeLabelsInventory g2
        degSeq1 = getDegreeSequence g1
        degSeq2 = getDegreeSequence g2
    in 
        v1 == v2 && e1 == e2 && nodeInv1 == nodeInv2 && edgeInv1 == edgeInv2 && degSeq1 == degSeq2

printNodes :: Show a => [LNode a] -> IO ()
printNodes [] = putStrLn "--- Конец списка ---"
printNodes ((nodeId, label) : rest) = do
    putStrLn $ "Вершина ID=" ++ show nodeId ++ " -> Метка: " ++ show label
    printNodes rest
