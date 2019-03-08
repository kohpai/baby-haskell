module Geometry.Cube
    ( volume
    , area
    )
where

import qualified Geometry.Cuboid               as Cuboid

volume :: Floating a => a -> a
-- cubeVolume x = x ^ 3
volume x = Cuboid.volume x x x

area :: Floating a => a -> a
-- cubeArea x = 6 * (x ^ 2)
area x = Cuboid.area x x x
