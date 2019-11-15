{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -Wno-unused-top-binds #-}

module Planar
    ( moveRight
    , moveLeft
    , Link (Link)
    , Vertex
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
newtype PlanarGraph = PG {
                        links :: [Link Vertex]
                      } deriving Show

-- TODO: This Eq is too restrictive. Need to allow for cyclic reorderings to be
-- equal.
newtype Link a = Link { linkPair :: (a, [a])} deriving (Show, Eq)
instance Functor Link where
  fmap f (Link xp) = Link (f $ fst xp, map f (snd xp))

newtype Face = Face {fvertices :: [Vertex]} deriving (Show, Ord)

instance Eq Face where
  f1 == f2 = sort (fvertices f1) == sort (fvertices f2)



pgVertices :: PlanarGraph -> [Vertex]
pgVertices pg = map linkVertex (links pg)


-- Functions for Link(s)

-- | Gets the link of the vertex in the graph
getLinkOf :: Vertex -> PlanarGraph -> Link Vertex
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


-- Get the face containing v1 and directed edge v1->v2 to the left of v1->v2
-- TODO: Make a test case for this! I'm on a roll, so don't feel like it now.
--
-- | Returns the face to the left of the direct edge v1->v2
faceLeftOf :: Vertex ->       -- ^ v1
              Vertex ->       -- ^ v2
              PlanarGraph ->  -- ^ pg
              Face
faceLeftOf v1 v2 pg = faceLeftOf' v1 v2 pg []
  where linkv1   = getLinkOf v1 pg
        lastVert = moveLeft v2 linkv1
        faceLeftOf' :: Vertex -> Vertex -> PlanarGraph -> [Vertex] -> Face
        faceLeftOf' v1' v2' pg' acc 
          | lastVert `elem` acc = Face acc
          | otherwise           = faceLeftOf' v2'
                                              nextVert 
                                              pg'
                                              (v1':acc)
          where linkv2 = getLinkOf v2' pg
                nextVert = moveRight v1' linkv2 -- Careful here!

-- | Returns all faces incident to the vertex v in pg.
findFacesAtV :: Vertex -> PlanarGraph -> [Face]
findFacesAtV v pg = [ faceLeftOf v v2 pg | v2<-linkVertices $ getLinkOf v pg ]

-- | Returns a list of all faces of the PG
pgFaces :: PlanarGraph -> [Face]
pgFaces pg = nub faces
  where faces = concat [findFacesAtV v pg | v<-pgVertices pg ]

-- | Returns the number of faces of the PG
numFaces :: PlanarGraph -> Int
numFaces = length . pgFaces

--LOLWUT. Both pgFaces and numFaces give Exception: divide by zero when applied
--to doubled tetrahedron..
--
-- Ok, I'm on to it. There are Vertex in some of the links of dc that do not
-- appear as vertices. Something is still wrong with the doubling.


-- Idea for doubling along a face:
-- 1) Reflect PG. Simply take all the same data and reverse the links
-- 2) Look at maximal index of og PG. Increase all vertices in the reflected PG
-- by this amount.
-- 3) Build new PG by fusing the face. This is definitely doable!
--
-- | Shift all vertex indices in the PG by n. Used for copying a PG.
shiftIndicesPG :: Int -> PlanarGraph -> PlanarGraph
shiftIndicesPG n pg = PG (shiftLinks n (links pg)) 

shiftLinks :: Int -> [Link Vertex] -> [Link Vertex]
shiftLinks n = map ((n+) <$>)

shiftFace :: Int -> Face -> Face
shiftFace n (Face vs) = Face (map (n+) vs)


-- | Flips the link for reflecting PG.
revLink :: Link Vertex -> Link Vertex
revLink (Link (v,vs)) = Link (v, reverse vs)

-- | Returns mirror image of PG.
reflectPG :: PlanarGraph -> PlanarGraph
reflectPG pg = PG (map revLink (links pg))


maxIndex :: PlanarGraph -> Int
maxIndex = maximum . pgVertices 

minIndex :: PlanarGraph -> Int
minIndex = minimum . pgVertices 

filterFace :: Face -> Link Vertex -> Bool
filterFace face link = linkVertex link `notElem` fvertices face

filterVertex :: Vertex -> Link Vertex -> Bool
filterVertex v link = linkVertex link /= v

linkOfFace :: Face -> PlanarGraph -> [Link Vertex]
linkOfFace face pg = [ l | l<-links pg,
                           linkVertex l `notElem` fvertices face,
                           any (\v -> v `elem` linkVertices l) (fvertices face) ] 

-- TODO: Remove this. It is the wrong thing to do. I'm just leaving it for now
-- since it surpresses the errors with the currently incorrect doublePG
fixLink :: Vertex -> Vertex -> Face -> PlanarGraph -> [Vertex]
fixLink n v f pg = sub (linkVertices (getLinkOf v pg))
  where sub [] = []
        sub (x:xs)
          | x `elem` fvertices f = (x + n) : sub xs 
          | otherwise = x : sub xs

-- This only makes sense for right-angled faces with all vertices degree 3
-- TODO: rewrite this to take advantage of updateLink.

doublePG :: PlanarGraph -> Face -> PlanarGraph
doublePG pg face = PG (nub $ lwofandnbrs ++ rlwofandnbrs ++ addedLinks ++ raddedLinks)
  where reflectedPg = shiftIndicesPG (maxIndex pg) (reflectPG pg) -- reflect pg
        reflFace = shiftFace (maxIndex pg) face
        linksWoFace = filter (filterFace face) (links pg) -- remove links with Vertex in face
        rlinksWoFace = filter (filterFace reflFace) (links reflectedPg) -- as above, but for reflected
        -- Remove all links centered at a vertex in the link of a vertex of face
        lwofandnbrs = filter (\l -> l `notElem` linkOfFace face pg) linksWoFace
        -- same, but for reflected
        rlwofandnbrs = filter (\l -> l `notElem` linkOfFace reflFace reflectedPg) rlinksWoFace
        -- I'm really not sure that fixLink is doing the right thing
        addedLinks = [ Link (a, fixLink (maxIndex pg) a face pg) | 
                            vf<-fvertices face,
                            a<-filter (\x -> x `notElem` fvertices face)
                                      (linkVertices (getLinkOf vf pg)) ]
        raddedLinks = [ Link (a, fixLink ((-1) * maxIndex pg) a reflFace reflectedPg) | 
                             vf<-fvertices reflFace,
                             a<-filter (\x -> x `notElem` fvertices reflFace)
                                       (linkVertices (getLinkOf vf reflectedPg)) ]

-- doublePG' :: PlanarGraph -> Face -> PlanarGraph
-- doublePG' pg face = PG (nub $ lwofandnbrs ++ rlwofandnbrs ++ addedLinks ++ raddedLinks)
  -- where reflectedPg = shiftIndicesPG (maxIndex pg) (reflectPG pg) -- reflect pg
        -- reflFace = shiftFace (maxIndex pg) face
        -- linksWoFace = filter (filterFace face) (links pg) -- remove links with Vertex in face
        -- rlinksWoFace = filter (filterFace reflFace) (links reflectedPg) -- as above, but for reflected
        -- -- Remove all links centered at a vertex in the link of a vertex of face
        -- lwofandnbrs = filter (\l -> l `notElem` linkOfFace face pg) linksWoFace
        -- -- same, but for reflected
        -- rlwofandnbrs = filter (\l -> l `notElem` linkOfFace reflFace reflectedPg) rlinksWoFace
        -- -- I'm really not sure that fixLink is doing the right thing
        -- addedLinks = [ Link (a, fixLink (maxIndex pg) a face pg) |
                            -- vf<-fvertices face,
                            -- a<-filter (\x -> x `notElem` fvertices face)
                                      -- (linkVertices (getLinkOf vf pg)) ]
        -- raddedLinks = [ Link (a, fixLink ((-1) * maxIndex pg) a reflFace reflectedPg) |
                             -- vf<-fvertices reflFace,
                             -- a<-filter (\x -> x `notElem` fvertices reflFace)
                                       -- (linkVertices (getLinkOf vf reflectedPg)) ]

        --                     /
        --                    /
        --    *--------------*---------------*
        --                   |
        --                   |
        --                   |
        --                   |
        --    *--------------*---------------*
        --    a              v              b
        --
        --    What I really want fixLink to do is not simply add to the index.
        --    It should leave from a towards the face and pop out on the other
        --    side. In the link of a, I want to replace v with b.


-- | returns the image of v when reflected a face. This is only relevant when
-- pg is a cubic graph. Which face is in fact irrelevant because when we do the
-- reflection, the opposite vertex is always going to be the one shifted by
-- maxIndex pg. `mod` (2*shift) makes it so that this works also for vertices in
-- the reflected copy of pg.
oppositeVertex :: Vertex -> PlanarGraph -> Vertex
oppositeVertex v pg = let shift = maxIndex pg - minIndex pg + 1
  in (v + shift) `mod` (2*shift)


-- I'm writing this just with links in pg in mind. Have to think about how to
-- extend to those in rpg.
--
-- Idea for above comment: It's symmetric, so how about just adding in the
-- reflected links as we work through. This'll take care of vertex duplication
-- too.
-- TODO: This really deserves a full suite of tests. (Also, it's 24 lines ;) )
-- TODO: A little flaw. My idea for doubling is to take all the links of pg and
-- rpg and then to just run updateLink over all of them. Since I was pg focused
-- when writing this, it won't really work on the rpg portion. Have to think
-- about how to fix that.
updateLink :: Link Vertex -> Face -> PlanarGraph -> Maybe (Link Vertex)
updateLink l f pg
  | lVlf `elem` fVerts && length lVslf == 3 = Nothing   -- linkVertex l in f is trivalent (Nothing)
  | lVlf `elem` fVerts && length lVslf > 3  = Just $ fuseLink l f pg -- linkVertex l in f is higher degree
  | l `elem` linkF && faceValence == 4 = 
      if lVlf `elem` pgVertices pg
        then Just l
        else Just (Link (linkVertex l, 
                   substitute (`elem` rfVerts) 
                              (`oppositeVertex` pg)
                              lVslf))                  -- l in link of face f, meets f at degree-4 vertex
  | l `elem` linkF && faceValence == 3 =            --  l in link of face f, meets f at degree-3 vertex
      Just (Link (linkVertex l,
            substitute (\v -> (v `elem` fVerts) ||
                              (v `elem` rfVerts))
                       (const $ oppositeVertex lVlf pg)
                       lVslf))
  | otherwise   = Just l                                -- l not in link of face f
  where linkF = linkOfFace f pg
        lVlf = linkVertex l
        lVslf = linkVertices l
        fVerts = fvertices f
        rfVerts = map (`oppositeVertex` pg) fVerts
        fv = head ( linkVertices l `intersect` fVerts)
        faceValence = linkValence (getLinkOf fv pg)

substitute :: (a -> Bool) -> (a -> a) -> [a] -> [a]
substitute _ _ [] = []
substitute pr f (x:xs)
  | pr x    = f x : substitute pr f xs
  | otherwise = x : substitute pr f xs


-- idea behind fuseLink is this: If l looks like
-- l = [a, b, f1, f2, c, d, e] (f1, f2 <- f), then it's reflected version is
-- rl = [c', f2', f1', b', a', e', d']
-- (In our construction, rl is actually more like l reversed, but things might
-- change. I'll just do the more general version.
--
-- We want the fusion to look like
-- [a, b, b', a', e', d', c', c, d, e] 
-- so that the fi's act like teleporters to the other list.
--
-- split l to [[a,b], [c,d,e]]
-- split rl = [[c'], [b',a',e',d']]. Then patch them up? They match up along
-- vertices that are reflections of the others.
--
-- | If linkVertex l is in f and the valence of linkVertex l > 3, then fuseLink
-- produces the new link replacing the link of linkVertex l in the double of pg.
fuseLink :: Link Vertex -> Face -> PlanarGraph -> Link Vertex
fuseLink l f pg = Link (linkVertex l, lPieces ++ rlPieces)
 where fVerts = fvertices f `intersect` linkVertices l
       rfVerts = map (`oppositeVertex` pg) fVerts
       teleporters = fVerts `union` map (`oppositeVertex` pg) fVerts
       lVerts  = linkVertices l
       rlVerts = map (`oppositeVertex` pg) lVerts
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



-- Tetrahedron (for testing purposes)
-- Note that the fact that Graph is directed has no effect whatsoever on what
-- we've done so far. Good. Just beware.
tetrahedron :: PlanarGraph
tetrahedron = PG [Link (1, [2,3,4]),
                  Link (2, [3,1,4]),
                  Link (3, [1,2,4]),
                  Link (4, [1,3,2])]

cube :: PlanarGraph
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

octahedron :: PlanarGraph
octahedron = PG [Link (1, [2, 5, 6, 3]),
                 Link (2, [3, 4, 5, 1]),
                 Link (3, [1 ,6 ,4 ,2]),
                 Link (4, [3 ,6 ,5 ,2]),
                 Link (5, [1 ,2 ,4 ,6]),
                 Link (6, [1 ,5 ,4 ,3])]


