module Main (main) where

import Lib 

main :: IO ()
main = do
    maybeGraph1 <- parseGraphFromFile "graph1.json"
    maybeGraph2 <- parseGraphFromFile "graph2.json"

    case (maybeGraph1, maybeGraph2) of
        (Just g1, Just g2) -> do
            
            print $ checkInvariants g1 g2 && isIsomorphic g1 g2

        (Nothing, _) -> putStrLn "\nError: Not reading graph1.json"
        (_, Nothing) -> putStrLn "\nError: Not reading graph2.json"
