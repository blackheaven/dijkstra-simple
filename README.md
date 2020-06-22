# dijkstra-simple

A simpler Dijkstra shortest paths implementation

## Basic usage
This section contains basic step-by-step usage of the library.

The first step is to build a direct graph:

```
exampleGraph :: Graph Char Int
exampleGraph = Graph $ M.fromList [
                                    ('A', [EdgeTo 'B' 3, EdgeTo 'C' 1])
                                  , ('B', [EdgeTo 'A' 3, EdgeTo 'C' 7, EdgeTo 'D' 5, EdgeTo 'E' 1])
                                  , ('C', [EdgeTo 'A' 1, EdgeTo 'B' 7, EdgeTo 'D' 2])
                                  , ('D', [EdgeTo 'B' 5, EdgeTo 'C' 2, EdgeTo 'E' 5])
                                  , ('E', [EdgeTo 'B' 1, EdgeTo 'D' 7])
                                  ]
```

Then pick or create a weighter (see `Graph.DijkstraSimple.Weighters`)
and apply it all:

```
lightestPaths exampleGraph 'C' weighter
```

It will give all the reacheable vertices from `'C'` and associated shortest path:

```
Paths $ M.fromList [
                     ('A', Path (fromList "AC") 1)
                   , ('B', Path (fromList "BAC") 3)
                   , ('C', Path (fromList "CAC") 1)
                   , ('D', Path (fromList "DC") 2)
                   , ('E', Path (fromList "EBAC") 3)
                   ]
```
