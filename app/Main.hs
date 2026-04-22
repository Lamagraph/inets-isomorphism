module Main (main) where

import Lib 

main :: IO ()
main = do
    putStrLn "--- Start LamaGraph Isomorphis ---"

    putStrLn "\n[1/2] Reading (graph1.json)..."
    maybeGraph1 <- parseGraphFromFile "graph1.json"

    putStrLn "[2/2] Reading graph2.json)..."
    maybeGraph2 <- parseGraphFromFile "graph2.json"

    case (maybeGraph1, maybeGraph2) of
        (Just g1, Just g2) -> do
            putStrLn "\nSuccess graph memory mapped!"
            
            putStrLn "\n=== Check invariants ==="
            print $ checkInvariants g1 g2

            putStrLn "\n=== Search isomorphism ==="
            print $ isIsomorphic g1 g2

        (Nothing, _) -> putStrLn "\nError: Not reading graph1.json"
        (_, Nothing) -> putStrLn "\nError: Not reading graph2.json"
