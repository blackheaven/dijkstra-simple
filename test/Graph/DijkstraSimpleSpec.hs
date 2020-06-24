module Graph.DijkstraSimpleSpec (main, spec) where

import Data.List.NonEmpty(fromList)
import qualified Data.Map.Lazy as M
import Test.Hspec

import Graph.DijkstraSimple
import Graph.DijkstraSimple.Weighters

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Graph.DijkstraSimple" $ do
    describe "lightestPaths" $ do
      it "classical cummulative weighter" $ do
        let paths = Paths $ M.fromList [
                                         ('A', Path (fromList "AC") 1)
                                       , ('B', Path (fromList "BAC") 4)
                                       , ('C', Path (fromList "CAC") 2)
                                       , ('D', Path (fromList "DC") 2)
                                       , ('E', Path (fromList "EBAC") 5)
                                       ]
        lightestPaths exampleGraph 'C' cumulativeWeighter `shouldBe` paths
      it "maximal weighter" $ do
        let paths = Paths $ M.fromList [
                                         ('A', Path (fromList "AC") 1)
                                       , ('B', Path (fromList "BAC") 3)
                                       , ('C', Path (fromList "CAC") 1)
                                       , ('D', Path (fromList "DC") 2)
                                       , ('E', Path (fromList "EBAC") 3)
                                       ]
        lightestPaths exampleGraph 'C' maximumWeightWeighter `shouldBe` paths
    describe "findPath" $ do
      it "unknown target" $
        findPath exampleGraph 'C' cumulativeWeighter 'U' `shouldBe` Nothing
      it "classical cummulative weighter" $
        findPath exampleGraph 'C' cumulativeWeighter 'E' `shouldBe` Just (Path (fromList "EBAC") 5)
      it "maximal weighter" $
        findPath exampleGraph 'C' maximumWeightWeighter 'E' `shouldBe` Just (Path (fromList "EBAC") 3)

exampleGraph :: Graph Char Int
exampleGraph = Graph $ M.fromList [
                                    ('A', [EdgeTo 'B' 3, EdgeTo 'C' 1])
                                  , ('B', [EdgeTo 'A' 3, EdgeTo 'C' 7, EdgeTo 'D' 5, EdgeTo 'E' 1])
                                  , ('C', [EdgeTo 'A' 1, EdgeTo 'B' 7, EdgeTo 'D' 2])
                                  , ('D', [EdgeTo 'B' 5, EdgeTo 'C' 2, EdgeTo 'E' 5])
                                  , ('E', [EdgeTo 'B' 1, EdgeTo 'D' 7])
                                  ]
