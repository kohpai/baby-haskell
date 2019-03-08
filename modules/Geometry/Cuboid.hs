module Geometry.Cuboid
    ( volume
    , area
    )
where

volume :: Num a => a -> a -> a -> a
volume x y z = x * y * z

area :: Num a => a -> a -> a -> a
area x y z = (2 * x * y) + (2 * y * z) + (2 * x * z)
