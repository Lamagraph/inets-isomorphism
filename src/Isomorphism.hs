module Isomorphism 
    ( isIsomorphic 
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Graph.Inductive
import Data.List (sort)
import Control.Monad.State
import Invariants (checkInvariants)

type Mapping = Map Node Node

data IsoState = IsoState 
    { core_1 :: Mapping  
    , core_2 :: Mapping  
    }

isFeasible :: (Eq a, Ord b) => Gr a b -> Gr a b -> IsoState -> Node -> Node -> Bool
isFeasible g1 g2 st n1 n2 = 
    checkLabels && checkDegrees && checkTopology
  where
    checkLabels = lab g1 n1 == lab g2 n2
    checkDegrees = (indeg g1 n1 == indeg g2 n2) && (outdeg g1 n1 == outdeg g2 n2)
    checkTopology = sort mappedOut1 == sort mappedOut2 && sort mappedIn1 == sort mappedIn2

    mappedOut1 = [ (core_1 st M.! dest, l) | (_, dest, l) <- out g1 n1, M.member dest (core_1 st) ]
    mappedOut2 = [ (dest, l) | (_, dest, l) <- out g2 n2, M.member dest (core_2 st) ]

    mappedIn1 = [ (core_1 st M.! src, l) | (src, _, l) <- inn g1 n1, M.member src (core_1 st) ]
    mappedIn2 = [ (src, l) | (src, _, l) <- inn g2 n2, M.member src (core_2 st) ]


matchNodes :: (Eq a, Ord b) => Gr a b -> Gr a b -> [Node] -> State IsoState Bool
matchNodes g1 g2 [] = return True
matchNodes g1 g2 (n1 : restG1) = do
    st <- get
    
    let candidatesG2 = filter (`M.notMember` core_2 st) (nodes g2)
    let validCandidates = filter (isFeasible g1 g2 st n1) candidatesG2
    
    tryCandidates validCandidates
  where
    tryCandidates [] = return False
    
    tryCandidates (n2 : n2s) = do
        oldState <- get
        
        modify $ \s -> s { core_1 = M.insert n1 n2 (core_1 s)
                         , core_2 = M.insert n2 n1 (core_2 s) }
        
        success <- matchNodes g1 g2 restG1
        
        if success
            then return True
            else do
                put oldState 
                tryCandidates n2s

isIsomorphic :: (Ord a, Ord b) => Gr a b -> Gr a b -> Bool
isIsomorphic g1 g2 =
    if not (checkInvariants g1 g2) 
    then False
    else evalState (matchNodes g1 g2 (nodes g1)) emptyState
  where
    emptyState = IsoState M.empty M.empty
