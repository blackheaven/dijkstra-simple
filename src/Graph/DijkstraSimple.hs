{-# LANGUAGE ScopedTypeVariables #-}

module Graph.DijkstraSimple (
                            -- * How to use this library
                            -- $use
                              lightestPaths
                            , EdgeTo(..)
                            , Graph(..)
                            , Weighter(..)
                            , Path(..)
                            , Paths(..)
                            )  where

import qualified Data.Map.Lazy as M
import Data.List.NonEmpty(NonEmpty(..), (<|), fromList)
import Data.Maybe(fromJust, isJust, isNothing)
import Data.Ord(comparing)
import qualified Data.PriorityQueue.FingerTree as P

-- | Edge to an arbitrary vertex and the associated input weight
data EdgeTo v e = EdgeTo { edgeTo :: v, edgeToWeight :: e } deriving (Eq, Show)

-- | All vertices and outgoing edges
newtype Graph v e = Graph { graphAsMap :: M.Map v [EdgeTo v e] } deriving (Eq, Show)

-- | Convert an input weight (edge-dependant) to an output weight
-- (path-dependant) for the algorithm work.
data Weighter v e a = Weighter { initialWeight :: a, weight :: EdgeTo v e -> Path v e a -> a }
-- | The lightest found path with reverse ordered list of traversed
-- vertices and output weight.
data Path v e a = Path { pathVertices :: NonEmpty v, pathWeight :: a } deriving (Eq, Show)
-- | Reachable vertices and associated lightest paths
newtype Paths v e a = Paths { pathsAsMap :: M.Map v (Path v e a) } deriving (Eq, Show)

type StatePQ v e a = P.PQueue a (Path v e a, EdgeTo v e)
type State v e a = (M.Map v (Path v e a), Maybe ((a, (Path v e a, EdgeTo v e)), StatePQ v e a))

-- | Explore all the reachable edges
lightestPaths :: forall v e a . (Ord v, Ord a) => Graph v e -> v -> Weighter v e a -> Paths v e a
lightestPaths graph origin weighter = Paths $ fst $ until (isNothing . snd) nextStep init
  where init :: State v e a
        init = (M.empty, P.minViewWithKey $ findEdges (Path (fromList [origin]) (initialWeight weighter)) origin)
        nextStep :: State v e a -> State v e a
        nextStep (paths, Just ((w, (path, e)), pq)) =
          let npq = if M.notMember (edgeTo e) paths
                     then pq `P.union` findEdges path (edgeTo e)
                     else pq
          in (M.alter (updatePath path) (edgeTo e) paths, P.minViewWithKey npq)
        updatePath :: Path v e a -> Maybe (Path v e a) -> Maybe (Path v e a)
        updatePath p prev =
          case prev of
            Nothing -> Just p
            Just op -> Just $ if pathWeight op <= pathWeight p
                                then op
                                else p
        findEdges :: Path v e a -> v -> StatePQ v e a
        findEdges path vertice = P.fromList
                                  $ map (buildPath path)
                                  $ M.findWithDefault [] vertice
                                  $ graphAsMap graph
        buildPath :: Path v e a -> EdgeTo v e -> (a, (Path v e a, EdgeTo v e))
        buildPath path e = let np = addEdge e path
                            in (pathWeight np, (np, e))
        addEdge :: EdgeTo v e -> Path v e a -> Path v e a
        addEdge e p = Path
                    { pathVertices = edgeTo e <| pathVertices p
                    , pathWeight = weight weighter e p
                    }

-- $use
--
-- This section contains basic step-by-step usage of the library.
--
-- The first step is to build a direct graph:
--
-- > exampleGraph :: Graph Char Int
-- > exampleGraph = Graph $ M.fromList [
-- >                                     ('A', [EdgeTo 'B' 3, EdgeTo 'C' 1])
-- >                                   , ('B', [EdgeTo 'A' 3, EdgeTo 'C' 7, EdgeTo 'D' 5, EdgeTo 'E' 1])
-- >                                   , ('C', [EdgeTo 'A' 1, EdgeTo 'B' 7, EdgeTo 'D' 2])
-- >                                   , ('D', [EdgeTo 'B' 5, EdgeTo 'C' 2, EdgeTo 'E' 5])
-- >                                   , ('E', [EdgeTo 'B' 1, EdgeTo 'D' 7])
-- >                                   ]
--
-- Then pick or create a weighter (see @Graph.DijkstraSimple.Weighters@)
-- and apply it all:
--
-- > lightestPaths exampleGraph 'C' weighter
--
-- It will give all the reacheable vertices from @'C'@ and associated shortest path:
--
-- > Paths $ M.fromList [
-- >                      ('A', Path (fromList "AC") 1)
-- >                    , ('B', Path (fromList "BAC") 3)
-- >                    , ('C', Path (fromList "CAC") 1)
-- >                    , ('D', Path (fromList "DC") 2)
-- >                    , ('E', Path (fromList "EBAC") 3)
-- >                    ]
