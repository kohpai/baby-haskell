-- Normal import
-- import           Data.List
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
  | let lengthList = map length xs in (> 1) $ length $ DataList.nub lengthList
  = error "all lists must have the same length"
  | otherwise
  = map sum $ DataList.transpose xs

allUpperCase :: String -> Bool
allUpperCase = DataList.all (`elem` ['A' .. 'Z'])

anyUpperCase :: String -> Bool
anyUpperCase = DataList.any (`elem` ['A' .. 'Z'])

geometricSeq = iterate (* 2) 1

firstThousandStock = head
  $ DataList.dropWhile (\(x, _, _, _) -> x < 1000) stock
 where
  stock =
    [ (994.4 , 2008, 9, 1)
    , (995.2 , 2008, 9, 2)
    , (999.2 , 2008, 9, 3)
    , (1001.4, 2008, 9, 4)
    , (998.3 , 2008, 9, 5)
    ]
-- firstThousandStock = head $ filter (\(x, _, _, _) -> x > 1000) stock
--   where
--     stock =
--         [ (994.4 , 2008, 9, 1)
--         , (995.2 , 2008, 9, 2)
--         , (999.2 , 2008, 9, 3)
--         , (1001.4, 2008, 9, 4)
--         , (998.3 , 2008, 9, 5)
--         ]

-- span, break, and splitAt

-- @TODO Why is this type declaration mandatory?
occuranceCount :: Ord a => [a] -> [(a, Int)]
occuranceCount =
  map (\all@(x : _) -> (x, length all)) . DataList.group . DataList.sort

-- startsWith :: Eq a => [a] -> [a] -> Bool
startWith hay needle = foldl (\acc x -> acc || needle == take nl x) False
  $ DataList.inits hay
  where nl = length needle
