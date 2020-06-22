module Graph.DijkstraSimpleSpec (main, spec) where

import Data.List.NonEmpty(fromList)
import qualified Data.Map.Lazy as M
import Test.Hspec

import Graph.DijkstraSimple

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Graph.DijkstraSimple" $ do
    it "classical cummulative weighter" $ do
      let paths = Paths $ M.fromList [
                                       ('A', Path (fromList "AC") 1)
                                     , ('B', Path (fromList "BAC") 4)
                                     , ('C', Path (fromList "CAC") 2)
                                     , ('D', Path (fromList "DC") 2)
                                     , ('E', Path (fromList "EBAC") 5)
                                     ]
      let weighter = Weighter 0 $ \e p -> pathWeight p + edgeToWeight e
      lightestPaths exampleGraph 'C' weighter `shouldBe` paths
    it "maximal weighter" $ do
      let paths = Paths $ M.fromList [
                                       ('A', Path (fromList "AC") 1)
                                     , ('B', Path (fromList "BAC") 3)
                                     , ('C', Path (fromList "CAC") 1)
                                     , ('D', Path (fromList "DC") 2)
                                     , ('E', Path (fromList "EBAC") 3)
                                     ]
      let weighter = Weighter minBound $ \e p -> max (pathWeight p) (edgeToWeight e)
      lightestPaths exampleGraph 'C' weighter `shouldBe` paths

exampleGraph :: Graph Char Int
exampleGraph = Graph $ M.fromList [
                                    ('A', [EdgeTo 'B' 3, EdgeTo 'C' 1])
                                  , ('B', [EdgeTo 'A' 3, EdgeTo 'C' 7, EdgeTo 'D' 5, EdgeTo 'E' 1])
                                  , ('C', [EdgeTo 'A' 1, EdgeTo 'B' 7, EdgeTo 'D' 2])
                                  , ('D', [EdgeTo 'B' 5, EdgeTo 'C' 2, EdgeTo 'E' 5])
                                  , ('E', [EdgeTo 'B' 1, EdgeTo 'D' 7])
                                  ]
