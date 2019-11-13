module Planar
    ( moveRight
    , moveLeft
    , Link (Link)
    , Vertex
    ) where

import Control.Applicative
import Data.Graph
import Data.List
import Data.Maybe

-- Plan: I want to encode a planar embedding of a graph. The idea is to use the
-- Data.Graph data structure and then to assign additional information to the
-- vertices + stuff. In particular, a planar embedding of a graph is determined
-- by 
--  1) a cyclic ordering of the edges incident at each vertex, and
--  2) a specification of the bounding cycle.
-- There are probably better data structures to use, but I'm just going to start
-- with using lists to represent these. (Two that seem likely: Data.CircularList
-- and Data.List.PointedList. Adding to the project is proving to be a major
-- headache right now. Just want proof of concept)

-- Maybe this?

data PlanarGraph = PG {
                        graph :: Graph,
                        links :: [Link],
                        bounding :: Bounding
                      } deriving Show

newtype Link = Link { linkPair :: (Vertex, [Vertex])} deriving Show
type Bounding = Face 



-- Functions for Link(s)

-- | Gets the link of the vertex in the graph
getLinkOf :: Vertex -> PlanarGraph -> Link
getLinkOf v pg = Link (v, fromMaybe [] (lookup v (map linkPair $ links pg)))

-- | Vertices in Link
linkVertices :: Link -> [Vertex]
linkVertices (Link (v, vs)) = vs

-- | Move right in the link:
moveRight :: Vertex -> -- ^ Current vertex in link
             Link   -> -- ^ Link we're working in
             Vertex    -- ^ Vertex to the right
moveRight e l = linkVertices l !! rIndex
  where eIndex = fromJust $ elemIndex e (linkVertices l)
        rIndex = (eIndex + 1) `mod` length (linkVertices l)

-- | Move left in the link:
moveLeft ::  Vertex -> -- ^ Current vertex in link
             Link   -> -- ^ Link we're working in
             Vertex    -- ^ Vertex to the left
moveLeft e l  = linkVertices l !! rIndex
  where eIndex = fromJust $ elemIndex e (linkVertices l)
        rIndex = (eIndex - 1) `mod` length (linkVertices l)


-- FACE stuff
newtype Face = Face {fvertices :: [Vertex]} deriving Show

-- instance Functor Face where
  -- fmap f (Face x) = Face (map f x)
-- Still not quite there.
-- instance Applicative Face where
  -- pure x = Face x
  -- (<*>) = liftA2


-- Get the face containing v1 and directed edge v1->v2 to the left of v1->v2
-- TODO: Make a test case for this! I'm on a roll, so don't feel like it now.
faceLeftOf :: Vertex ->       -- ^ v1
              Vertex ->       -- ^ v2
              PlanarGraph ->  -- ^ pg
              Face
faceLeftOf v1 v2 pg = faceLeftOf' v1 v2 pg []
  where linkv1   = getLinkOf v1 pg
        lastVert = moveLeft v2 linkv1
        faceLeftOf' :: Vertex -> Vertex -> PlanarGraph -> [Vertex] -> Face
        faceLeftOf' v1 v2 pg acc 
          | lastVert `elem` acc = Face acc
          | otherwise           = faceLeftOf' v2
                                              nextVert 
                                              pg 
                                              (v1:acc)
          where linkv2 = getLinkOf v2 pg
                nextVert = moveRight v1 linkv2 -- Careful here!

-- | Returns all faces incident to the vertex v in pg.
findFacesAtV :: Vertex -> PlanarGraph -> [Face]
findFacesAtV v pg = [ faceLeftOf v v2 pg | v2<-linkVertices $ getLinkOf v pg ]


-- Tetrahedron (for testing purposes)
-- Note that the fact that Graph is directed has no effect whatsoever on what
-- we've done so far. Good. Just beware.
tetrahedron :: PlanarGraph
tetrahedron = PG gr
                 lks
                 bdd
                 where gr = buildG (1, 4) [ (1,2), (1,3), 
                                            (1,4), (2,3), 
                                            (2,4), (3,4) ]
                 -- where gr = buildG (1, 4) [ (1,2), (2,1), (1,3), (3,1),
                                            -- (1,4), (4,1), (2,3), (3,2),
                                            -- (2,4), (4,2), (3,4), (4,3) ]
                       lks = [Link (1, [2,3,4]),
                              Link (2, [3,1,4]),
                              Link (3, [1,2,4]),
                              Link (4, [1,3,2])]
                       bdd = Face [2,3,4]


