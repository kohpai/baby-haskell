-- Hellog
import           Data.Ratio

log11 :: Num a => a -> Double
log11 = logBase 11

threeToTenM :: Num a => Int -> a
threeToTenM = (3 ^) . (10 *)

twoSomething :: Num a => Int -> a
twoSomething = (2 *) . (3 ^) . (5 *)

isInt x = x == fromInteger (round x)

interM :: Int -> Ratio Integer
interM m = ((%) $ toInteger $ (threeToTenM m -) $ twoSomething m + 1) 4

y :: Int -> Double
y m = log11 $ (/ 4) $ toInteger $ (threeToTenM m -) $ twoSomething m + 1

ys = [ (x, m) | m <- [1 ..], let x = y m, isInt x ]
