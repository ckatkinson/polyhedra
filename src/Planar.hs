module Planar
    ( moveRight
    , moveLeft
    , Link (Link)
    ) where

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

data PlanarGraph = PG Graph Links Bounding deriving Show

newtype Link = Link (Vertex, [Vertex]) deriving Show
newtype Links = Links [Link] deriving Show
newtype Bounding = Bounding [Vertex] deriving Show



-- Functions for Link(s)

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
