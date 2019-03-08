module Geometry.Sphere
    ( volume
    , area
    )
where

volume :: Floating a => a -> a
volume r = 4.0 / 3.0 * pi * (r ^ 3)

area :: Floating a => a -> a
area r = 4 * pi * (r ^ 2)
