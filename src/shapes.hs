module Shape(Shape, rectangle, rtTriangle, ellipse, polygon, direction, normalToSegmen,
  dot, onLeft, convex) where

import Data.List.Index

data Shape =
  Rectangle Side Side
  | Ellipse Radius Radius
  | RtTriangle Side Side
  | Polygon [Vertex]
  deriving Show

type Radius = Double
type Side = Double
type Vertex = (Double, Double)
type Vector = (Double, Double)

data VertexData = VData Vertex Vertex [Vertex] deriving Show

rectangle :: Side -> Side -> Shape
rectangle a b = Rectangle a b

rtTriangle :: Side -> Side -> Shape
rtTriangle a b = RtTriangle a b

ellipse :: Radius -> Radius -> Shape
ellipse a b = Ellipse a b

polygon :: [Vertex] -> Shape
polygon vs = Polygon vs



-- OPERATIONS AND FUNCTIONS

direction :: Vertex -> Vertex -> Vector
direction (x1, y1) (x2, y2) =
  (x2 - x1, y2 - y1)

rotate90 :: Vector -> Vector
rotate90 (x, y) = (-y, x)

normalToSegment :: Vertex -> Vertex -> Vector
normalToSegment from to =
  rotate90 (direction from to)

dot :: Vector -> Vector -> Double
dot (x1, y1) (x2, y2) =
  x1*x2 + y1*y2

{-
  > a = (0,0); b = (2,0); c = (1,1)
  > onLeft a b c
    True

  > c' = (1,-1)
  > onLeft a b c'
    False

-}
onLeft :: Vertex -> Vertex -> Vertex -> Bool
onLeft from to v =
  let
    n = normalToSegment from to
    d = direction from v
  in
    dot d n > 0

{-
  > prepareOne 0 vs
    VData ((0.0,0.0),(1.0,0.0)) [(2.0,0.0),(3.0,0.0),(4.0,0.0),(5.0,0.0)]

  > prepareOne 2 vs
    VData ((2.0,0.0),(3.0,0.0)) [(0.0,0.0),(1.0,0.0),(4.0,0.0),(5.0,0.0)]

  > prepareOne 5 vs
    VData ((5.0,0.0),(0.0,0.0)) [(1.0,0.0),(2.0,0.0),(3.0,0.0),(4.0,0.0)]

-}
prepareOne :: Int -> [Vertex] -> VertexData
prepareOne k_ vs =
  let
    n = length vs
    k = k_ `mod` n -- constrain to range of allowable indices
    from = vs !! k
    to = vs !! ((k + 1) `mod` n)
    vs' = if k < n - 1
            then take k vs ++ drop (k + 2) vs
            else drop 1 (take k vs)
  in
    VData from to vs'

prepare :: [Vertex] -> [VertexData]
prepare vs =
  let
    n = length vs
    ks = [0..(n - 1)]
  in
    fmap (\k -> prepareOne k vs) ks


andAll :: [Bool] -> Bool
andAll [] = True
andAll (b:bs) = b && andAll bs


verticesOnLeft :: VertexData -> Bool
verticesOnLeft (VData from to vlist) =
  andAll (fmap (onLeft from to) vlist)


convex' :: [Vertex] -> Bool
convex' vs =
  andAll (fmap verticesOnLeft (prepare vs))

convex :: Shape -> Bool
convex shape =
  case shape of
    Rectangle _ _ -> True
    Ellipse _ _ -> True
    RtTriangle _ _ -> True
    Polygon vList -> convex' vList

-- TEST DATA

quad = Polygon vs
quad' = Polygon vs'

{- Vertices of convex quadrilateeral -}
vs :: [Vertex]
vs = [(0,0), (3,0), (3,3), (0,3)]

{- Vertices of nonconvex quadrilateeral -}
vs' :: [Vertex]
vs' = [(0,0), (3,0), (1,2), (0,3)]
