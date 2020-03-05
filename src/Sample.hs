{-# LANGUAGE NoMonomorphismRestriction #-}
module Sample where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
import Data.GraphViz

-- From
-- https://hackage.haskell.org/package/diagrams-graphviz-1.4.1.1/docs/Diagrams-TwoD-GraphViz.html
--


hex = mkGraph [0..19] 
    (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ] 
     ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ] 
     ++ [ (2,18,()), (2,19,()), (15,18,()),(15,19,()), (18,3,()), (19,3,()) ])

atest = theGraph >>= defaultMain
  where
    theGraph :: IO (Diagram B)
    theGraph = simpleGraphDiagram Dot hex
      
