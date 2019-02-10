import qualified Data.List as DataList

numUniques :: (Eq a) => [a] -> Int
numUniques = length . DataList.nub

stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]

highStock :: (Num a, Ord a) => [(a, Integer, Integer, Integer)] -> [(a, Integer, Integer, Integer)]
highStock xs = filter (\x -> (fst' x) > 1000) xs
    where
        fst' :: (Num a) => (a, Integer, Integer, Integer) -> a
        fst' (w,x,y,z) = w
