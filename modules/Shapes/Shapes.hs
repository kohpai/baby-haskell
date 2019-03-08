-- write your type along with the functions you are exporting
-- and then add some parentheses and in them specify the value constructors
-- that you want to export for it, separated by commas.
-- If you want to export all the value constructors for a given type,
-- just write ..
module Shapes
    ( Point(..)
    , Shape(..)
    , surface
    , nudge
    , baseCircle
    , baseRect
    )
where

-- We could also opt not to export any value constructors for Shape
-- by just writing Shape in the export statement.
-- Also, whoever uses our module can't pattern match
-- against the value constructors.

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
    (abs $ x2 - x1) * (abs $ y2 - y1)

-- Value constructors are functions,
-- so we can map them and partially apply them and everything
-- circles = map (Circle (Point 10 20)) [4, 5, 6, 6]

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x1 y1) r) x y = (Circle $ (Point $ x1 + x) $ y1 + y) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) x y =
    (Rectangle $ (Point $ x1 + x) $ y1 + y) $ (Point $ x2 + x) $ y2 + y

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect w l = Rectangle (Point 0 0) (Point w l)
