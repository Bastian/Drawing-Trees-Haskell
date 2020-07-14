module DrawingTrees
(
  Tree (..),
  PositionedTree,
  design
) where

-- A simple tree data
data Tree a = Node a [Tree a] deriving (Show, Read, Eq)

-- A tree with position information
type PositionedTree a = Tree (a, Float)

-- Moves the given tree
moveTree :: Float -> PositionedTree a -> PositionedTree a
moveTree offset (Node (label, curPos) subtrees) = Node (label, curPos + offset) subtrees

-- Type alias for the extent
type Extent = [(Float, Float)]

-- Moves the extent horizontally
moveExtent :: Float -> Extent -> Extent
moveExtent offset = map (\(p, q) -> (p + offset, q + offset))

-- Merges two non-overlapping extents
-- Example:
-- merge [(1,1),(0,2),(0,3)] [(5,5),(4,6),(4,7)]
-- > [(1,5),(0,6),(0,7)]
merge :: Extent -> Extent -> Extent
merge [] qs = qs
merge ps [] = ps
merge ((p,_):ps) ((_,q):qs) = (p, q) : merge ps qs

-- Like merge but for a list of extents
mergeList :: [Extent] -> Extent
mergeList = foldl merge []

-- Calculates the minimum distance between two non-overlapping extents, assuming
-- a minumum distance of 1 between nodes.
-- Example:
-- fit [(1,1),(0,2),(0,3)] [(1,1),(0,2),(0,3)]
-- > 4.0
fit :: Extent -> Extent -> Float
fit ((_,p):ps) ((q,_):qs) = max (fit ps qs) (p - q + 1)
fit _          _          = 0

-- Calculates the minumum distance for a list of extents, relative to
-- the leftmost tree.
-- Example:
-- fitlistl [[(1,1),(0,2),(0,3)], [(1,1),(0,2),(0,3)], [(1,1),(0,2),(0,3)]]
-- > [0.0,4.0,8.0]
fitlistl :: [Extent] -> [Float]
fitlistl es = fitlistl' [] es
  where
    fitlistl' :: Extent -> [Extent] -> [Float]
    fitlistl' acc [] = []
    fitlistl' acc (e:es) =
      let x = fit acc e
      in x : fitlistl' (merge acc (moveExtent x e)) es

-- Calculates the minumum distance for a list of extents, relative to
-- the rightmost tree.
-- Example:
-- fitlistr [[(1,1),(0,2),(0,3)], [(1,1),(0,2),(0,3)], [(1,1),(0,2),(0,3)]]
-- > [-8.0,-4.0,-0.0]
fitlistr :: [Extent] -> [Float]
fitlistr = reverse . (map negate) . fitlistl . (map flipextend) . reverse
  where flipextend = map (\(p, q) -> (negate q, negate p))

-- Combines right and left firlist variants by calculating the mean.
-- Example:
-- fitlist [[(1,1),(0,2),(0,3)], [(1,1),(0,2),(0,3)], [(1,1),(0,2),(0,3)]]
-- > [-4.0,0.0,4.0]
fitlist :: [Extent] -> [Float]
fitlist es = map mean $ zip (fitlistl es) (fitlistr es)
  where mean (x, y) = (x + y) / 2

-- Designs a positioned tree for the given tree
design :: Tree a -> PositionedTree a
design tree = fst $ design' tree
  where
    -- Auxiliary function to prevent unnecessary recalculations of the extent
    design' (Node label subtrees) = (resulttree, resultextent)
      where
        -- 1. Design the subtrees recursively
        (trees, extents) = unzip $ map design' subtrees
        -- 2. Fit the extents of the deisgned trees
        positions        = fitlist extents
        -- 3. Move the designed trees by the corresponding displacement
        ptrees           = map (uncurry $ moveTree) $ zip positions trees
        -- 4. Also move the extents by the corresponding displacement
        pextents         = map (uncurry $ moveExtent) $ zip positions extents
        -- 5. Calculate the final extent
        resultextent     = (0,0) : (mergeList pextents)
        -- 6. Create the final tree (with root node at position 0)
        resulttree       = Node (label, 0) ptrees
