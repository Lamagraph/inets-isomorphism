module Main (main) where

import Data.Graph.Inductive.Graph (mkGraph, labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Invariants

main :: IO ()
main = do
    let nodes1 = [ (1, "Альфа"), (2, "Бета"), (3, "Гамма") ]
    let edges1 = [ (1, 2, 100), (2, 3, 200) ]
    let graph1 :: Gr String Int
        graph1 = mkGraph nodes1 edges1
    
    let nodes2 = [ (10, "Гамма"), (20, "Альфа"), (30, "Бета") ]
    let edges2 = [ (20, 30, 100), (30, 10, 200) ]
    let graph2 :: Gr String Int
        graph2 = mkGraph nodes2 edges2
    
    putStrLn "=== Граф 1 ==="
    printNodes (labNodes graph1)

    putStrLn "\n=== Граф 2 ==="
    printNodes (labNodes graph2)

    putStrLn "\n=== Проверка инвариантов (Граф 1 и Граф 2) ==="
    print $ checkInvariants graph1 graph2

