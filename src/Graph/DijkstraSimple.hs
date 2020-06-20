{-# LANGUAGE ScopedTypeVariables #-}

module Graph.DijkstraSimple (
                              shortestPaths
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

data EdgeTo v e = EdgeTo { edgeTo :: v, edgeToWeight :: e } deriving (Eq, Show)
newtype Graph v e = Graph { graphAsMap :: M.Map v [EdgeTo v e] } deriving (Eq, Show)

data Weighter v e a = Weighter { initialWeight :: a, weight :: EdgeTo v e -> Path v e a -> a }
data Path v e a = Path { pathEdges :: NonEmpty v, pathWeight :: a } deriving (Eq, Show)
newtype Paths v e a = Paths { pathsAsMap :: M.Map v (Path v e a) } deriving (Eq, Show)

type StatePQ v e a = P.PQueue a (Path v e a, EdgeTo v e)
type State v e a = (M.Map v (Path v e a), Maybe ((a, (Path v e a, EdgeTo v e)), StatePQ v e a))

shortestPaths :: forall v e a . (Ord v, Ord a, Show a, Show v, Show e) => Graph v e -> v -> Weighter v e a -> Paths v e a
shortestPaths graph origin weighter = Paths $ fst $ until (isNothing . snd) nextStep init
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
                    { pathEdges = edgeTo e <| pathEdges p
                    , pathWeight = weight weighter e p
                    }
