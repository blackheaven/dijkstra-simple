{-# LANGUAGE ScopedTypeVariables #-}

module Graph.DijkstraSimple.Weighters
  (
                                      -- * How weighters are used
                                      -- $use
    cumulativeWeighter
  , maximumWeightWeighter
  )
where

import           Graph.DijkstraSimple

-- | The classical weighter: the weight of a path is the sum of each edge
-- weight.
cumulativeWeighter :: Num e => Weighter v e e
cumulativeWeighter = Weighter 0 $ \e p -> pathWeight p + edgeToWeight e

-- | Here we are looking for the heaviest edge weight
maximumWeightWeighter :: (Bounded e, Ord e) => Weighter v e e
maximumWeightWeighter =
  Weighter minBound $ \e p -> max (pathWeight p) (edgeToWeight e)

-- $use
--
-- Weighters requires two components:
--
--  - a value for the initial weight of a @Path@
--  - a function which gives a new output weight from an input weight (the edge weight) and a @Path@
--
-- The algorithm will try to minimize the output weight of paths.
--
-- Be sure that the output weight is always positive, it is not checked,
-- but it will break the algorithm.
