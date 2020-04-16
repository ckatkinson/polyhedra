{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS -Wno-unused-top-binds #-}

module Planar
    ( moveRight
    , moveLeft
    , numFaces
    , pgFaces
    , pgVertices
    , pgEdges
    , mmaEdge
    , mkMma
    , doublePG
    , Link (Link)
    , Vertex
    , PlanarGraph (PG)
    , Face
    , pgCube
    , tetrahedron
    , octahedron
    , gvGraph
    , drawOut
    , doublings
    ) where

import Data.List
import Data.List.Split (splitOneOf)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz

import Data.GraphViz
import Data.Graph.Inductive.PatriciaTree

-- Plan: I want to encode a planar embedding of a graph. The idea is to use the
-- Data.Graph data structure and then to assign additional information to the
-- vertices + stuff. In particular, a planar embedding of a graph is determined
-- by 
--  1) a cyclic ordering of the edges incident at each vertex, and
--  2) a specification of the bounding cycle.
-- There are probably better data structures to use, but I'm just going to start
-- with using lists to represent these. (Two that seem likely: Data.CircularList
-- and Data.List.PointedList.)


type Vertex           = Int
newtype PlanarGraph a = PG {
                        links :: [Link a]
                           } deriving Show

type Edge = (Vertex, Vertex)
type GvEdge = (Vertex, Vertex, ())


instance Functor PlanarGraph where
  fmap f (PG ls) = PG (map (fmap f) ls)

type Graph = PlanarGraph Vertex

-- TODO: This Eq is too restrictive. Need to allow for cyclic reorderings to be
-- equal. 
newtype Link a = Link { linkPair :: (a, [a])} deriving (Show, Eq)
instance Functor Link where
  fmap f (Link xp) = Link (f $ fst xp, map f (snd xp))

newtype FFace a = Face {fvertices :: [a]} deriving (Show, Ord)

instance (Eq a, Ord a) => Eq (FFace a) where
  f1 == f2 = sort (fvertices f1) == sort (fvertices f2)

instance Functor FFace where
  fmap f (Face xs) = Face (map f xs)

type Face = FFace Vertex




pgVertices :: Graph -> [Vertex]
pgVertices pg = map linkVertex (links pg)

-- | Reindex a PG's vertices sequentially starting from 1
reindexPG :: Graph -> Graph
reindexPG pg = rein <$> pg
  where indexAssoc = zip (pgVertices pg) [1..]
        rein v = fromJust $ lookup v indexAssoc

-- Functions for Link(s)

-- | Gets the link of the vertex in the graph
getLinkOf :: Vertex -> Graph -> Link Vertex
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
moveRight :: Vertex      -> -- ^ Current vertex in link
             Link Vertex -> -- ^ Link we're working in
             Vertex         -- ^ Vertex to the right
moveRight e l  = linkVertices l !! rIndex
  where eIndex = fromJust $ elemIndex e (linkVertices l)
        rIndex = (eIndex + 1) `mod` length (linkVertices l)

-- | Move left in the link:
moveLeft ::  Vertex      -> -- ^ Current vertex in link
             Link Vertex -> -- ^ Link we're working in
             Vertex         -- ^ Vertex to the left
moveLeft e l   = linkVertices l !! rIndex
  where eIndex = fromJust $ elemIndex e (linkVertices l)
        rIndex = (eIndex - 1) `mod` length (linkVertices l)

-- EDGE stuff

-- | Gives list of edges of graph.
-- We only want to get each edge once, so it suffices to just add edges (v,w)
-- where w > v. 
pgEdges :: Graph -> [Edge]
pgEdges pg = concatMap (\v -> [(v, w) | w <- linkBigger v]) pgVerts
    where pgVerts = pgVertices pg
          linkBigger :: Vertex -> [Vertex]
          linkBigger v = filter ( > v) (linkVertices $ getLinkOf v pg)

-- FACE stuff


-- | Returns the face to the left of the direct edge v1->v2
faceLeftOf :: Vertex -> -- ^ v1
              Vertex -> -- ^ v2
              Graph  -> -- ^ pg
              Face
faceLeftOf v1 v2 pg = faceLeftOf' v1 v2 pg []
  where linkv1   = getLinkOf v1 pg
        lastVert = moveLeft v2 linkv1
        faceLeftOf' :: Vertex -> Vertex -> Graph -> [Vertex] -> Face
        faceLeftOf' v1' v2' pg' acc 
          | lastVert `elem` acc = Face acc
          | otherwise           = faceLeftOf' v2'
                                              nextVert 
                                              pg'
                                              (v1':acc)
          where linkv2   = getLinkOf v2' pg
                nextVert = moveRight v1' linkv2

-- | Returns all faces incident to the vertex v in pg.
findFacesAtV :: Vertex -> Graph -> [Face]
findFacesAtV v pg = [ faceLeftOf v v2 pg | v2<-linkVertices $ getLinkOf v pg ]


-- | Returns all faces adjacent to f in pg
adjacentFaces :: Face -> Graph -> [Face]
adjacentFaces f pg =  filter 
                      (\fa -> length (fvertices fa `intersect` fvertices f) > 1) $
                      delete f $
                      nub $ concat [findFacesAtV v pg | v <- fvertices f]

-- | Returns as set of all faces adjacent to f in pg
adjacentFacesSet :: Face -> Graph -> S.Set Face
adjacentFacesSet f pg = S.fromList $ adjacentFaces f pg 



-- | Returns a list of all faces of the PG
pgFaces :: Graph -> [Face]
pgFaces pg = nub faces -- This is likely slow. Could consider a set here.
  where faces = concat [findFacesAtV v pg | v<-pgVertices pg ]

-- | Maximum degree of a face
maxFaceDegree :: Graph -> Int
maxFaceDegree g = maximum $ map (length . fvertices) (pgFaces g)

-- | Returns the number of faces of the PG
numFaces :: Graph -> Int
numFaces = length . pgFaces

-- | Shifts indices of vertices in the link l by n
shiftLink :: Int -> Link Vertex -> Link Vertex
shiftLink n l = (n+) <$> l

-- | Flips the link for reflecting PG.
revLink :: Link Vertex -> Link Vertex
revLink (Link (v,vs)) = Link (v, reverse vs)

-- | Maximum vertex index in graph
maxIndex :: Graph -> Int
maxIndex = maximum . pgVertices 


-- | Union of links of vertices that lie in the link of a vertex of face (but
-- not in face)
linkOfFace :: Face -> Graph -> [Link Vertex]
linkOfFace face pg = [ l | l<-links pg,
                           linkVertex l `notElem` fvertices face,
                           any (\v -> v `elem` linkVertices l) (fvertices face) ] 

-- | Doubles right-angled polyhedron pg along the face f. Works for polyhedra
-- having degree 3 and 4 vertices. 
doublePG :: Graph -> Face -> Graph
--doublePG pg f = reindexPG $ PG ( concatMap (makeLinks pg f) (links pg)) 
doublePG pg f = PG ( concatMap (makeLinks pg f) (links pg)) 


-- | returns the image of v when reflected a face. This is only relevant when
-- pg is a cubic graph. 
oppositeVertex :: Vertex -> Graph -> Vertex
oppositeVertex v pg  
  | v <= maxIndex pg = v + maxIndex pg 
  | otherwise        = v - maxIndex pg


-- | makeLinks looks at a link l of a pg and constructs 0, 1, or 2 links
-- corresponding to what links come from l in the double of pg
makeLinks :: Graph -> Face -> Link Vertex -> [Link Vertex]
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
substitute p f (x:xs)
  | p x    = f x : substitute p f xs
  | otherwise = x : substitute p f xs


--
-- | If linkVertex l is in f and the valence of linkVertex l > 3, then fuseLink
-- produces the new link replacing the link of linkVertex l in the double of pg.
fuseLink :: Link Vertex -> Face -> Graph -> Link Vertex
fuseLink l f pg = Link (linkVertex l, lPieces ++ rlPieces)
 where fVerts = fvertices f `intersect` linkVertices l
       rfVerts = map (`oppositeVertex` pg) fVerts
       teleporters = fVerts `Data.List.union` map (`oppositeVertex` pg) fVerts
       lVerts  = linkVertices l
       rlVerts = reverse $ map (`oppositeVertex` pg) lVerts
       lPieces = head $ filter (not . null) $ splitOneOf teleporters (faceHead lVerts fVerts)
       rlPieces = head $ filter (not . null) $ splitOneOf teleporters (faceHead rlVerts rfVerts)

-- | Rotate list so that a given sublist is the beginning.
faceHead :: Eq a => [a] -> -- input list
                    [a] -> -- what should be at the front
                    [a]
faceHead [] _ = []
faceHead inp frt
  | head inp `elem` frt = inp
  | otherwise           = faceHead (tail inp ++ [head inp]) frt


-- | Check of a list is a sublist of another.
isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf [] _      = True
isSublistOf (x:xs) ys  
  | x `elem` ys = isSublistOf xs ys
  | otherwise   = False

-- | Check of a face of xs doubles to a face of ys
isDerivedFaceOf :: Face -> Face -> Bool
isDerivedFaceOf (Face xs) (Face ys) = isSublistOf xs ys

-- | returns the face in the graph g that "comes from" f where we're thinking of
-- f as being a face in a previous doubling and the graph g as being a doubling
-- of the og graph
derivedFace :: Face -> Graph -> Face
derivedFace f g = snd $ head theBface
  where faces = pgFaces g
        bFaces = zip (map (f `isDerivedFaceOf`) faces) faces
        theBface = filter fst bFaces


-- | Given a list of faces of a pg, produces the pg obtained by successively
-- doubling along the faces derived from those in the list.
doublingSeq :: Graph -> [Face] -> Graph
doublingSeq g [] = g
doublingSeq g (f:fs) = doublingSeq (doublePG g df) fs
  where df = derivedFace f g

-- | Gives sequence of face numbers of graphs obtained by doubling along faces
-- in the list of faces.
faceSeq :: Graph -> [Face] -> [Int]
faceSeq g [] = [numFaces g]
faceSeq g (f:fs) = numFaces doubg : faceSeq doubg fs
  where df = derivedFace f g
        doubg = doublePG g df

-- TODO Goal: I want to automate the selection of faces. Girao's paper uses an
-- alternating black/white sequence of faces (these are right-angled ideal, so
-- we can always 2-color the faces black/white). Given a pg, how can I keep
-- track of a coloring?
--
-- I guess I want a type ColoredGraph that has both a pg and a function 
--   color :: Face -> Color
-- satisfying the condition that whenever f1 and f2 are adjacent, 
-- color f1 /= color f2.
--

data FaceColor = Wh | Bl deriving (Show, Eq)

data ColoredGraph = CG 
                       { planarGraph :: !Graph
                       , coloring    :: Map Face FaceColor
                       }


instance Show ColoredGraph where
  show (CG gr col) = show [(f, c f) | f <- pgFaces gr]
    where c f = fromJust $ Map.lookup f col

-- | Returns the mod2 distance between faces in the dual graph of planar graph.
-- I'm using mod2 because I only care about this for coloring. Removes the need
-- to minimize.
--
-- Essentially, this is a scan starting from the source face, expanding an
-- annular region of faces until one finds the target face.

mod2FaceDistance :: Face -> Face -> Graph -> Integer
mod2FaceDistance fsource ftarget pg = mod2FaceDistance' fsource ftarget pg (S.fromList [fsource])
  where
    mod2FaceDistance' fs ft g seen
      | ft `S.member` seen = 0
      | otherwise = (1 + mod2FaceDistance' fs ft g newSeen) `mod` 2
        where diskSeen = S.union seen $
                          S.unions $ S.map (`adjacentFacesSet` g) seen
              newSeen = S.difference diskSeen seen

-- Produce a colored graph from a graph with a seed face (the seed will be
-- colored Wh. Unprimed version is the best one.
--

colorPGSeed' gr seed = CG gr colorMap
  where 
    colorMap = Map.fromList [(f,c f) | f <- pgFaces gr] 
    c f 
      | mod2FaceDistance f seed gr == 0 = Wh
      | otherwise                       = Bl

colorPGSeed :: Graph -> Face -> ColoredGraph
colorPGSeed gr seed = colorPGSeed'' gr initFaces initMap Wh
  where 
    initFaces = S.difference (S.fromList (pgFaces gr)) (S.singleton seed)
    initMap = Map.fromList [(seed, Wh)]
    colorPGSeed'' :: Graph -> Set Face -> Map Face FaceColor -> FaceColor -> ColoredGraph
    colorPGSeed'' gg facesRemaining colors lastColor
      | S.null facesRemaining = CG gg colors
      | otherwise             = colorPGSeed'' gg 
                                              (S.difference facesRemaining newAnnulus)
                                              (Map.union colors newColors)
                                              (nextColor lastColor)
      -- color everything in newAnnulus. Remove everything in newAnnulus from facesRemaining
      where coloredFaces = Map.keysSet colors
            newAnnulus = S.difference withAdjacent coloredFaces
            withAdjacent = S.union coloredFaces $
                             S.unions $ S.map (`adjacentFacesSet` gg) coloredFaces
            nextColor Wh = Bl
            nextColor Bl = Wh
            newColors = Map.fromSet (const (nextColor lastColor)) newAnnulus


-- Girao co-final tower construction:
-- Plan:
--  1) Given initial polyhedron, 2-color it. Use the 2-coloring to choose the
--  correct sequence of faces along which to double
--  2) use the sequence doubler to double along the selected faces
--  3) recolor and repeat.


interleaveLists :: [[a]] -> [a]
interleaveLists = concat . transpose

partitionFaces :: ColoredGraph -> ([Face], [Face])
partitionFaces cg = partition isWhite faces
  where faces = pgFaces $ planarGraph cg
        isWhite f = fromJust (Map.lookup f (coloring cg)) == Wh

oneStepGiraoSeq :: ColoredGraph -> [Face]
oneStepGiraoSeq cg = interleaveLists $ listify $ partitionFaces cg
  where listify pair = [fst pair, snd pair]

giraoSeq :: ColoredGraph -> [Face]
giraoSeq cg = oneStepGiraoSeq cg ++ giraoSeq cog
  where cog = colorPGSeed os (head $ pgFaces os)
        os = oneStepGiraoDoublings (planarGraph cg)

giraoNumFaceSeq :: Graph -> [Int]
giraoNumFaceSeq gr = faceSeq gr $ giraoSeq cgr
  where cgr = colorPGSeed gr (head $ pgFaces gr)


oneStepGiraoDoublings :: Graph -> Graph
oneStepGiraoDoublings gr = doublingSeq gr faces
  where faces = oneStepGiraoSeq (colorPGSeed gr (head $ pgFaces gr))

-- DRAWING

-- make the graph Below is the graphViz code. Doesn't seem to do a good job with
-- planarity. Futher below is code to produce Mathematica input. Works much better!
--
mkGvEdge :: Edge -> GvEdge
mkGvEdge (v,w) = (v,w,())

gvGraph :: Planar.Graph -> Data.Graph.Inductive.PatriciaTree.Gr Vertex ()
gvGraph pg = mkGraph (pgVertices pg) (map mkGvEdge (pgEdges pg))

drawOut :: Graph -> IO ()
drawOut pg = do
  let params :: GraphvizParams Int v e () v
      params = defaultDiaParams
               { fmtEdge = const [arrowTo noArrow] }
  gr' <- layoutGraph' params Neato (gvGraph pg)
  let grDrawing :: Diagram B
      grDrawing = drawGraph
                     (const $ place (circle 9))
                     (\_ _ _ _ _ p -> stroke p)
                     gr'
  mainWith $ grDrawing # frame 1

-- Mathematica version. When compiling, it's convenient to write a main function
-- so that you can just pipe this into pbcopy.

mmaEdge :: Edge -> String
mmaEdge e = "{" ++ show (fst e) ++ "," ++ show (snd e) ++ "}"

mkMma :: Graph -> IO ()
--mkMma pg = putStrLn $ "Graph[Rule @@@ {" ++ edgs ++ "}, GraphLayout -> \"PlanarEmbedding\"]"
mkMma pg = putStrLn $ beg ++ edgs ++ finish
  where edgs = intercalate "," (map mmaEdge $ pgEdges pg)
        beg = "PlanarGraph[{"
        finish = "}, VertexLabels -> \"Name\"]"


-- Examples below

tetrahedron :: Graph
tetrahedron = PG [Link (1, [2,3,4]),
                  Link (2, [3,1,4]),
                  Link (3, [1,2,4]),
                  Link (4, [1,3,2])]

pgCube :: Graph
pgCube= PG [Link (1, [2,3,5]),
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

octahedron :: Graph
octahedron = PG [Link (1, [2, 5, 6, 3]),
                 Link (2, [3, 4, 5, 1]),
                 Link (3, [1 ,6 ,4 ,2]),
                 Link (4, [3 ,6 ,5 ,2]),
                 Link (5, [1 ,2 ,4 ,6]),
                 Link (6, [1 ,5 ,4 ,3])]

coctahedron = colorPGSeed octahedron (Face [1,2,3])

fiveDrum :: Graph
fiveDrum = PG [Link (1, [3, 9, 10, 2]),
               Link (3, [5, 1, 2, 4]),
               Link (5, [7, 3, 4, 6]),
               Link (7, [9, 5, 6, 8]),
               Link (9, [1, 7, 8, 10]),
               Link (2, [3, 1, 10, 4]),
               Link (4, [5, 3, 2, 6]),
               Link (6, [7, 5, 4, 8]),
               Link (8, [9, 7, 6, 10]),
               Link (10, [1, 9, 8, 2])]

cFiveDrum = colorPGSeed fiveDrum (head $ pgFaces fiveDrum)
              

doublings :: IO ()
doublings = do -- let girao = iterate oneStepGiraoDoublings octahedron
               -- let ngirao = map numFaces girao
               -- print $ take 3 ngirao
               -- let octs = iterate (\p -> doublePG p (head $ pgFaces p)) octahedron
               -- let nocts = map numFaces octs
               -- print $ take 20 nocts
-- doublings = do mkMma octahedron
               -- let doct = doublePG octahedron (Face [6,5,4])
               -- mkMma doct
               -- let ddoct = doublePG doct (Face [2,4,8,5])
               -- mkMma ddoct
               -- let dddoct = doublePG ddoct (Face [1,3,6])
               -- mkMma dddoct
               -- let d4oct = doublePG dddoct (Face [1,2,3,20])
               -- mkMma d4oct
               -- let d5oct = oneStepGiraoDoublings octahedron
               -- let d6oct = oneStepGiraoDoublings d5oct
               -- mkMma d6oct
               -- mkMma d5oct
               let drummy = oneStepGiraoDoublings fiveDrum
               mkMma drummy




