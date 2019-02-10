-- Expressions return something after they are evaluated,
-- while statements are just actions that computers perform
-- If, let binding, and case are expressions
-- There are no while loops or for loops in Haskell and instead
-- we many times have to use recursion to declare what something is.

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

-- In case you're not sure about the type declaration of a function,
-- use :t <func> in ghci
replaceOdd :: (Ord a, Num a) => a -> String
replaceOdd n = if n < 10 then "BOOM!" else "BANG!"

-- 1. x is drawn from list xs
-- 2. x is filtered through the predicate (odd x)
-- 3. x is subjected to the output function (replaceOdd x)
--
-- Note: multiple lists can be used, as well as multiple
-- predicates (filter functions)
boomBangs xs = [ replaceOdd x | x <- xs, odd x ]

-- This is a common pattern in functional programming. You take a starting set
-- of solutions and then you apply transformations to those solutions and
-- filter them until you get the right ones.
triangles =
    [ (a, b, c)
    | c <- [1 .. 10]
    , a <- [1 .. c]
    , b <- [1 .. a]
    , a ^ 2 + b ^ 2 == c ^ 2
    , a + b + c == 24
    ]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z'] ]

-- Type annotation
-- Since read returns a value of any types that belong to Read type class
-- (which has so many types), it needs to know which type it should return
-- Try this in ghci `read "5" :: Int`

-- Pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

-- factorial :: (Integral a) => a -> a
-- factorial 0 = 1
-- factorial n = n * factorial (n-1)

-- a is just a type!!! Not a variable
fst3 :: [a] -> (a, a, a)
--fat3 xs = case xs of (x:y:z:xs) -> (x, y, z)
--                     xs -> error "You need at lease 3 elems in the list"
fst3 (x : y : z : xs) = (x, y, z)
fst3 xs               = error "You need at lease 3 elems in the list"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

third :: (a, b, c) -> c
third (_, _, z) = z

-- Pattern matching in list comprehensions
-- let xs = [(1, 3), (4, 3), (2, 4), (5, 3), (5, 6), (3, 1)]
-- ptmatchedList = [a+b | (a, b) <- xs]

-- length' xs = sum [1 | _ <- xs]
length' :: (Integral b) => [a] -> b
length' []       = 0
length' (_ : xs) = 1 + length' xs

-- foldx1, they assume the first (or last) element of the list to be
-- the starting value and then start the fold with the element next to it.
sum' :: (Num a) => [a] -> a
-- sum' [] = 0
-- sum' (x:xs) = x + sum' xs
sum' = foldl1 (+)

-- Get all the list via the variable all
fstLetter :: String -> String
fstLetter ""           = "Empty string, whoops!"
fstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- The where defines values which can be accessed within
-- the pattern of a function
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height | bmi <= skinny = "Gain some weight"
                      | bmi <= normal = "Stay there"
                      | bmi <= fat    = "Lose some weight"
                      | otherwise     = "There's no return"
  where
    bmi                   = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25, 30)

max' :: (Ord a) => a -> a -> a
max' a b | a < b     = b
         | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b | a > b     = GT
               | a == b    = EQ
               | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

-- `let` bindings are expressions themselves, while `where` bindings are
-- just syntactic constructs, it cannot be used across guards
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^ 2
    in  sideArea + 2 * topArea

testLet1 = 4 * (let a = 9 in a + 1) + 2
testLet2 = let square x = x * x in [square 5, square 3, square 2]
testLet3 =
    ( let a = 100
          b = 200
          c = 300
      in  a * b * c
    , let foo = "Hey "
          bar = "there!"
      in  foo ++ bar
    )

-- Using pattern matching to assign multiple values at once in very handy
-- let (a, b, c) = (100, 200, 300) in a+b+c

-- The names defined in a let inside a list comprehension are visible to the output function 
-- and all predicates and sections that come after of the binding
fatBmi :: (RealFloat a) => [(a, a)] -> [a]
-- fatBmi xs = [w / h^2 | (w, h) <- xs, w / h^2 >= 25.0]
fatBmi xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0 ]

-- Pattern matching on parameters in function definitions is actually
-- just syntactic sugar for case expressions.
-- They are useful for pattern matching against something in the middle of
-- an expression
-- describeList :: [a] -> String
-- describeList xs = "The list is " ++ case xs of
--     []  -> "an empty list."
--     [x] -> "a singleton list."
--     xs  -> "a long list."

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
  where
    what []  = "an empty list."
    what [x] = "a singleton list."
    what xs  = "a long list."

maximum' :: (Ord a) => [a] -> a
maximum' []       = error "The list cannot be empty!"
maximum' [x     ] = x
-- maximum' (x:xs) = if maximum' [x] > maxrest then x else maxrest
-- maximum' (x:xs)
--     | x > maxrest = x 
--     | otherwise = maxrest
--     where
--         maxrest = maximum' xs
maximum' (x : xs) = max x (maximum' xs)

replicate' :: Int -> b -> [b]
replicate' 0 _ = []
replicate' x y | x < 0     = error "Are you out of your mind?!"
               | otherwise = y : replicate' (x - 1) y

take' :: Int -> [a] -> [a]
take' _ [] = []
take' x (h : xs) | x <= 0    = []
                 | otherwise = h : take' (x - 1) xs

-- The : operator (also called the cons operator) is instantaneous
reverse' :: [a] -> [a]
-- reverse' [] = []
-- reverse' xs = last xs : reverse' (init xs)
reverse' = foldl (flip (:)) []

repeat' :: a -> [a]
repeat' x = x : repeat x

zip' :: [a] -> [b] -> [(a, b)]
-- zip' xs ys
--     | null xs || null ys = []
--     | otherwise = (x, y):zip' xtail ytail 
--     where
--         (x:xtail) = xs
--         (y:ytail) = ys
zip' []       _        = []
zip' _        []       = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
-- _ `elem'` [] = False
-- x `elem'` (h:xs)
--     | x == h = True
--     | otherwise = x `elem'` xs
elem' x = foldl (\b y -> b || (x == y)) False
-- elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

quicksort :: (Ord a) => [a] -> [a]
quicksort []       = []
quicksort (x : xs) = smallerList ++ [x] ++ biggerList
  where
    smallerList = let es = filter (<= x) xs in quicksort es
    biggerList  = let es = filter (> x) xs in quicksort es
    -- smallerList = quicksort [ e | e <- xs, e <= x ]
    -- biggerList  = quicksort [ e | e <- xs, e > x ]

-- Partially applied function
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
multWithEighteen = multThree 2 9

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyOnce :: (a -> a) -> (a -> a)
applyOnce f = f
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []       _        = []
zipWith' _ _        []       = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- Because functions are curried by default, which means
-- (a -> b -> c) -> (b -> a -> c) is the same as
-- (a -> b -> c) -> b -> a -> c
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--     where
--        g x y = f y x
--flip' f = \x y -> f y x

largestDivisible =
    let p x = x `mod` 3829 == 0 in head (filter p [100000, 99999 ..])
sumOddSquares = sum $ takeWhile (< 10000) $ map (^ 2) [1, 3 ..]

collatzSequence :: Integer -> [Integer]
collatzSequence 1 = [1]
collatzSequence x | even x    = x : collatzSequence (x `div` 2)
                  | otherwise = x : collatzSequence (x * 3 + 1)

-- Lambdas are basically anonymous functions that are used because we need
-- some functions only once. Normally, we make a lambda with the sole
-- purpose of passing it to a higher-order function.
addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

lenMoreThan15 :: Int
lenMoreThan15 =
    length $ filter ((> 15) . length) $ map collatzSequence [1 .. 100]

listOfFuncs = map (*) [0 ..]

-- Right folds work on infinite list, while left folds don't!
-- To put it plainly, if you take an infinite list at some point and
-- you fold it up from the right, you'll eventually reach the beginning of
-- the list. However, if you take an infinite list at a point and you try
-- to fold it up from the left, you'll never reach an end!
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

sumRootsUnder1000 =
    (+1) $ length $ takeWhile (< 1000) $ scanl1 (+) $ map sqrt [1 ..]

-- map function application over a list of partially applied functions
-- WTF!!!
listFuncApp = map ($ 3) [(+ 4), (10 *), (^ 2), sqrt]

-- One of the uses for function composition is making
-- functions on the fly to pass to other functions.
-- Function composition is right-associative
toNegative :: [Int] -> [Int]
toNegative = map (negate . abs)

-- chaining functions using composition
complexExp = replicate 100 . product . map (* 3) . zipWith max [1, 2, 3, 4, 5] $ [4, 5, 6, 7, 8]

-- point-free style function definition
complexExp2 = ceiling . negate . tan . cos . max 50

-- The reason why $ doesn't work like . because when using $, Haskell will
-- try to apply all the functions together, which is not what we want. We
-- want it to create a new function.
