-- Normal import
import           Data.List
import           Data.Function
import           Data.Char
-- Import only some functions
-- import Data.List (nub, sort) 
-- Import all except ...
-- import Data.List hiding (nub) 
-- Solving name conflicts
-- import qualified Data.Map
-- import qualified Data.List                     as DL
-- import qualified Data.Function                 as DF
-- import qualified Data.Char                     as DC
import qualified Data.Map                      as Map

-- nub takes out the duplicate elements
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

sumPolynomials :: [[Int]] -> [Int]
sumPolynomials []   = []
sumPolynomials [[]] = []
sumPolynomials xs
  | let lengthList = map length xs in (> 1) $ length $ nub lengthList = error
    "all lists must have the same length"
  | otherwise = map sum $ transpose xs

allUpperCase :: String -> Bool
allUpperCase = all (`elem` ['A' .. 'Z'])

anyUpperCase :: String -> Bool
anyUpperCase = any (`elem` ['A' .. 'Z'])

geometricSeq = iterate (* 2) 1

stock =
  [ (994.4 , 2008, 9, 1)
  , (995.2 , 2008, 9, 2)
  , (999.2 , 2008, 9, 3)
  , (1001.4, 2008, 9, 4)
  , (998.3 , 2008, 9, 5)
  ]

firstThousandStock = find (\(x, _, _, _) -> x > 1000) stock
-- firstThousandStock =
--   head $ dropWhile (\(x, _, _, _) -> x < 1000) stock
-- firstThousandStock = head $ filter (\(x, _, _, _) -> x > 1000) stock

-- span, break, and splitAt

-- @TODO Why is this type declaration mandatory?
occuranceCount :: Ord a => [a] -> [(a, Int)]
occuranceCount = map (\all@(x : _) -> (x, length all)) . group . sort

-- startsWith :: Eq a => [a] -> [a] -> Bool
xWith hay needle alg = foldl (\acc x -> acc || needle == take nl x) False
  $ alg hay
  where nl = length needle

startWith hay needle = xWith hay needle inits
endsWith hay needle = xWith hay needle tails

oddOnesOut = partition (`elem` ['A' .. 'Z']) "FlUoCvKeyYouOU"

cleanSentence =
  unwords $ words "hey these           are    the words in this\nsentence"

complement = [1 .. 10] \\ [2, 3, 4]

groupPolar = groupBy ((==) `on` (< 0)) values
 where
  values =
    [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]

sortByLength :: [[a]] -> [[a]]
-- sortByLength = sortBy
--   (\x y ->
--     let xl = length x
--         yl = length y
--     in  if xl < yl then LT else if xl == yl then EQ else GT
--   )
sortByLength = sortBy (compare `on` length)

isValidUsername :: String -> Bool
isValidUsername = all isAlphaNum

-- words' = filter (/= " ") . groupBy ((==) `on` isSpace)
words' = filter (not . any isSpace) . groupBy ((==) `on` isSpace)

encode :: Int -> String -> String
encode key msg = map chr $ map (+ key) $ map ord msg

decode :: Int -> String -> String
-- decode key msg = map chr $ map (+ negate key) $ map ord msg
decode key msg = encode (negate key) msg

-- using head instead of [list] !! 0
-- using snd instead of pattern matching
findKey :: String -> Maybe String
-- findKey key | null left = Nothing
--             | otherwise = Just $ snd $ head left
findKey key = foldl (\acc (k, v) -> if k == key then Just v else acc)
                    Nothing
                    phoneBook
 where
  -- left = filter (\(k, _) -> k == key) phoneBook
  phoneBook =
    [ ("betty"  , "555-2938")
    , ("bonnie" , "452-2928")
    , ("patsy"  , "493-2928")
    , ("lucille", "205-2928")
    , ("wendy"  , "939-8282")
    , ("penny"  , "853-2492")
    ]
