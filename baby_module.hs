-- Import only some functions
-- import Data.List (nub, sort) 
-- Import all except ...
-- import Data.List hiding (nub) 
-- Solving name conflicts
-- import qualified Data.Map
import qualified Data.List                     as DataList

-- nub takes out the duplicate elements
numUniques :: (Eq a) => [a] -> Int
numUniques = length . DataList.nub

sumPolynomials :: [[Int]] -> [Int]
sumPolynomials []   = []
sumPolynomials [[]] = []
sumPolynomials xs
    | let lengthList = map length xs
      in  (> 1) $ length $ DataList.nub lengthList
    = error "all lists must have the same length"
    | otherwise
    = map sum $ DataList.transpose xs

-- stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]

-- highStock :: (Num a, Ord a) => [(a, Integer, Integer, Integer)] -> [(a, Integer, Integer, Integer)]
-- highStock xs = filter (\x -> (fst' x) > 1000) xs
--     where
--         fst' :: (Num a) => (a, Integer, Integer, Integer) -> a
--         fst' (w,x,y,z) = w
