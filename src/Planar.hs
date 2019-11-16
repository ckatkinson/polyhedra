{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -Wno-unused-top-binds #-}

module Planar
    ( moveRight
    , moveLeft
    , numFaces
    , pgFaces
    , pgVertices
    , doublePG
    , Link (Link)
    , Vertex
    , PlanarGraph (PG)
    , Face
    ) where

import Data.List
import Data.List.Split (splitOneOf)
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

type Vertex = Int
newtype PlanarGraph a = PG {
                        links :: [Link a]
                      } deriving Show

instance Functor PlanarGraph where
  fmap f (PG ls) = PG (map (fmap f) ls)

-- TODO: This Eq is too restrictive. Need to allow for cyclic reorderings to be
-- equal. 
newtype Link a = Link { linkPair :: (a, [a])} deriving (Show, Eq)
instance Functor Link where
  fmap f (Link xp) = Link (f $ fst xp, map f (snd xp))

newtype Face = Face {fvertices :: [Vertex]} deriving (Show, Ord)

instance Eq Face where
  f1 == f2 = sort (fvertices f1) == sort (fvertices f2)



pgVertices :: PlanarGraph Vertex -> [Vertex]
pgVertices pg = map linkVertex (links pg)

-- | Reindex a PG's vertices sequentially starting from 1
reindexPG :: PlanarGraph Vertex -> PlanarGraph Vertex
reindexPG pg = rein <$> pg
  where indexAssoc = zip (pgVertices pg) [1..]
        rein v = fromJust $ lookup v indexAssoc


-- Functions for Link(s)

-- | Gets the link of the vertex in the graph
getLinkOf :: Vertex -> PlanarGraph Vertex -> Link Vertex
getLinkOf v pg = Link (v, fromMaybe [] (lookup v (map linkPair $ links pg)))

-- | Vertex at which Link is centered
linkVertex :: Link Vertex -> Vertex
linkVertex (Link (v, _)) = v

-- | Vertices in Link
linkVertices :: Link Vertex -> [Vertex]
linkVertices (Link (_, vs)) = vs

-- | degree of a vertex
linkValence :: Link Vertex -> Int
linkValence = length . linkVertices


-- | Move right in the link:
moveRight :: Vertex -> -- ^ Current vertex in link
             Link Vertex   -> -- ^ Link we're working in
             Vertex    -- ^ Vertex to the right
moveRight e l = linkVertices l !! rIndex
  where eIndex = fromJust $ elemIndex e (linkVertices l)
        rIndex = (eIndex + 1) `mod` length (linkVertices l)

-- | Move left in the link:
moveLeft ::  Vertex -> -- ^ Current vertex in link
             Link Vertex   -> -- ^ Link we're working in
             Vertex    -- ^ Vertex to the left
moveLeft e l  = linkVertices l !! rIndex
  where eIndex = fromJust $ elemIndex e (linkVertices l)
        rIndex = (eIndex - 1) `mod` length (linkVertices l)


-- FACE stuff

faceLength :: Face -> Int
faceLength = length . fvertices


-- | Returns the face to the left of the direct edge v1->v2
faceLeftOf :: Vertex ->       -- ^ v1
              Vertex ->       -- ^ v2
              PlanarGraph Vertex ->  -- ^ pg
              Face
faceLeftOf v1 v2 pg = faceLeftOf' v1 v2 pg []
  where linkv1   = getLinkOf v1 pg
        lastVert = moveLeft v2 linkv1
        faceLeftOf' :: Vertex -> Vertex -> PlanarGraph Vertex -> [Vertex] -> Face
        faceLeftOf' v1' v2' pg' acc 
          | lastVert `elem` acc = Face acc
          | otherwise           = faceLeftOf' v2'
                                              nextVert 
                                              pg'
                                              (v1':acc)
          where linkv2 = getLinkOf v2' pg
                nextVert = moveRight v1' linkv2 -- Careful here!

-- | Returns all faces incident to the vertex v in pg.
findFacesAtV :: Vertex -> PlanarGraph Vertex -> [Face]
findFacesAtV v pg = [ faceLeftOf v v2 pg | v2<-linkVertices $ getLinkOf v pg ]

-- | Returns a list of all faces of the PG
pgFaces :: PlanarGraph Vertex -> [Face]
pgFaces pg = nub faces
  where faces = concat [findFacesAtV v pg | v<-pgVertices pg ]

-- | Returns the number of faces of the PG
numFaces :: PlanarGraph Vertex -> Int
numFaces = length . pgFaces

-- | Shift all vertex indices in the PG by n. Used for copying a PG.
shiftIndicesPG :: Int -> PlanarGraph Vertex -> PlanarGraph Vertex
shiftIndicesPG n pg = PG (shiftLinks n (links pg)) 

shiftLinks :: Int -> [Link Vertex] -> [Link Vertex]
shiftLinks n = map ((n+) <$>)

shiftLink :: Int -> Link Vertex -> Link Vertex
shiftLink n l = (n+) <$> l

shiftFace :: Int -> Face -> Face
shiftFace n (Face vs) = Face (map (n+) vs)


-- | Flips the link for reflecting PG.
revLink :: Link Vertex -> Link Vertex
revLink (Link (v,vs)) = Link (v, reverse vs)

-- | Returns mirror image of PG.
reflectPG :: PlanarGraph Vertex -> PlanarGraph Vertex
reflectPG pg = PG (map revLink (links pg))


maxIndex :: PlanarGraph Vertex -> Int
maxIndex = maximum . pgVertices 

minIndex :: PlanarGraph Vertex -> Int
minIndex = minimum . pgVertices 

filterFace :: Face -> Link Vertex -> Bool
filterFace face link = linkVertex link `notElem` fvertices face

filterVertex :: Vertex -> Link Vertex -> Bool
filterVertex v link = linkVertex link /= v

linkOfFace :: Face -> PlanarGraph Vertex -> [Link Vertex]
linkOfFace face pg = [ l | l<-links pg,
                           linkVertex l `notElem` fvertices face,
                           any (\v -> v `elem` linkVertices l) (fvertices face) ] 


-- TODO (MAJOR TODO). Test this carefully.
doublePG :: PlanarGraph Vertex -> Face -> PlanarGraph Vertex
doublePG pg f = reindexPG $ PG ( concatMap (makeLinks pg f) (links pg)) 


-- | returns the image of v when reflected a face. This is only relevant when
-- pg is a cubic graph. Which face is in fact irrelevant because when we do the
-- reflection, the opposite vertex is always going to be the one shifted by
-- maxIndex pg. `mod` (2*shift) makes it so that this works also for vertices in
-- the reflected copy of pg.
oppositeVertex :: Vertex -> PlanarGraph Vertex -> Vertex
oppositeVertex v pg  
  | v <= maxIndex pg = v + maxIndex pg 
  | otherwise        = v - maxIndex pg


-- | makeLinks looks at a link l of a pg and constructs 0, 1, or 2 links
-- corresponding to what links come from l in the double of pg
makeLinks :: PlanarGraph Vertex -> Face -> Link Vertex -> [Link Vertex]
makeLinks pg f l
  | lVlf `elem` fVerts && length lVslf == 3 = []   -- linkVertex l in f is trivalent (Nothing)
  | lVlf `elem` fVerts && length lVslf > 3  = [fuseLink l f pg] -- linkVertex l in f is higher degree
  | l `elem` linkF && faceValence == 4 = [l, Link (oppositeVertex lVlf pg, 
                                                   reverse $
                                                   substitute (`notElem` fVerts) 
                                                              (`oppositeVertex` pg)
                                                              lVslf)]     -- l in link of face f, meets f at degree-4 vertex
  | l `elem` linkF && faceValence == 3 =            --  l in link of face f, meets f at degree-3 vertex
      [Link (linkVertex l,
            substitute (`elem` fVerts)
                       (const $ oppositeVertex lVlf pg)
                       lVslf),
       Link (oppositeVertex lVlf pg,
             reverse $
             map link3 lVslf)]
  | otherwise   = [l, revLink $ shiftLink (maxIndex pg) l]                                -- l not in link of face f
  where linkF = linkOfFace f pg
        lVlf = linkVertex l
        lVslf = linkVertices l
        fVerts = fvertices f
        fv = head ( linkVertices l `intersect` fVerts)
        faceValence = linkValence (getLinkOf fv pg)
        link3 v 
          | v `elem` fVerts = linkVertex l 
          | otherwise = oppositeVertex v pg

-- | subsitutes elements satisfying a predicate in a list via a function
substitute :: (a -> Bool) -> (a -> a) -> [a] -> [a]
substitute _ _ [] = []
substitute pr f (x:xs)
  | pr x    = f x : substitute pr f xs
  | otherwise = x : substitute pr f xs


--
-- | If linkVertex l is in f and the valence of linkVertex l > 3, then fuseLink
-- produces the new link replacing the link of linkVertex l in the double of pg.
fuseLink :: Link Vertex -> Face -> PlanarGraph Vertex -> Link Vertex
fuseLink l f pg = Link (linkVertex l, lPieces ++ rlPieces)
 where fVerts = fvertices f `intersect` linkVertices l
       rfVerts = map (`oppositeVertex` pg) fVerts
       teleporters = fVerts `union` map (`oppositeVertex` pg) fVerts
       lVerts  = linkVertices l
       rlVerts = reverse $ map (`oppositeVertex` pg) lVerts
       lPieces = head $ filter (not . null) $ splitOneOf teleporters (faceHead lVerts fVerts)
       rlPieces = head $ filter (not . null) $ splitOneOf teleporters (faceHead rlVerts rfVerts)

-- Even better for simplicity: Rotate so that the face vertices are are at
-- head/last

faceHead :: Eq a => [a] -> -- input list
                    [a] -> -- what should be at the front
                    [a]
faceHead [] _ = []
faceHead inp frt
  | head inp `elem` frt = inp
  | otherwise           = faceHead (tail inp ++ [head inp]) frt

tetrahedron :: PlanarGraph Vertex
tetrahedron = PG [Link (1, [2,3,4]),
                  Link (2, [3,1,4]),
                  Link (3, [1,2,4]),
                  Link (4, [1,3,2])]

cube :: PlanarGraph Vertex
cube= PG [Link (1, [2,3,5]),
          Link (2, [1,6,4]),
          Link (3, [1,4,7]),
          Link (4, [2,8,3]),
          Link (5, [1,7,6]),
          Link (6, [2,5,8]),
          Link (7, [3,8,5]),
          Link (8, [4,6,7])]



--                 1
--
--               
--               5----6
--                \ /
--                 4
--
--        2               3
--

octahedron :: PlanarGraph Vertex
octahedron = PG [Link (1, [2, 5, 6, 3]),
                 Link (2, [3, 4, 5, 1]),
                 Link (3, [1 ,6 ,4 ,2]),
                 Link (4, [3 ,6 ,5 ,2]),
                 Link (5, [1 ,2 ,4 ,6]),
                 Link (6, [1 ,5 ,4 ,3])]




