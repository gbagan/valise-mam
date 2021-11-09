module Lib.Graph where

import MyPrelude

import Data.Array (deleteAt)

data Edge = Edge Int Int
infix 3 Edge as ↔
instance Eq Edge where
    eq (u1 ↔ v1) (u2 ↔ v2) = u1 == u2  && v1 == v2 || u1 == v2 && u2 == v1

type Position = { x ∷ Number, y ∷ Number }

-- | une structure Graph est composé d'un titre, d'une liste des arêtes et de la position de chaque sommet dans le plan
type Graph = {title ∷ String, vertices ∷ Array Position, edges ∷ Array Edge }

getCoords ∷ Graph → Int → Maybe Position
getCoords graph u = graph.vertices !! u

getCoordsOfEdge ∷ Graph → Edge → Maybe {px1 ∷ Number, px2 ∷ Number, py1 ∷ Number, py2 ∷ Number}
getCoordsOfEdge graph (u ↔ v) = do
    {x: px1, y: py1} ← getCoords graph u
    {x: px2, y: py2} ← getCoords graph v
    pure {px1, px2, py1, py2}

addVertex ∷ Position → Graph → Graph
addVertex pos graph = graph { vertices = graph.vertices `snoc` pos }

removeVertex ∷ Int → Graph → Graph
removeVertex i graph = graph {  vertices = graph.vertices # deleteAt i # fromMaybe graph.vertices
                             ,  edges = graph.edges # mapMaybe \(u ↔ v) → 
                                    if u == i || v == i then
                                        Nothing
                                    else
                                        Just $ (if u > i then u - 1 else u) ↔ (if v > i then v - 1 else v)
                             }

removeEdge ∷ Int → Int → Graph → Graph
removeEdge u v graph = graph {  edges = graph.edges # filter (_ /= u ↔ v) }

moveVertex ∷ Int → Position → Graph → Graph
moveVertex i pos graph = graph { vertices = graph.vertices # updateAtIndices [i /\ pos] }

addEdge ∷ Int → Int → Graph → Graph
addEdge u v graph = graph { edges = if u == v || (u ↔ v) `elem` graph.edges then
                                        graph.edges
                                    else
                                        graph.edges `snoc` (u ↔ v)
                          }

