import Data.List

-- *Main> is_zero 4
-- False
is_zero :: Int -> Bool
is_zero 0 = True
is_zero _ = False

-- *Main> asc 1 7
-- [1,2,3,4,5,6,7]
asc :: Int -> Int -> [Int]
asc n m
    | m < n = []
    | m == n = [m]
    | m > n = n : asc (n+1) m

-- *Main> mySum [21,543,234,3265,234]
-- 4297
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- *Main> evens [3,675,2,3,7,4,2]
-- [2,4,2]
evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
    | mod x 2 == 0 = x : evens xs
    | otherwise    = evens xs

-- *Main> myFst (4,6)
-- 4
myFst :: (a,b) -> a
myFst (x,_) = x

-- *Main> mySnd (24,78234)
-- 78234
mySnd :: (a,b) -> b
mySnd (_,y) = y

-- *Main> addTuples [(5,6), (23,76), (73,7)]
-- [11,99,80]
addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [ x+y | (x,y) <- xs ]

-- *Main> myelem 2 [2,5,2,67]
-- True
-- *Main> myelem 2 [5,67]
-- False
myelem :: (Eq a) => a -> [a] -> Bool
myelem _ [] = False
myelem e (x:xs) = (e == x) || (myelem e xs)

-- *Main> myNub [2,6,3,8,3,2]
-- [6,8,3,2]
myNub :: (Eq a) => [a] -> [a]
myNub [] = []
myNub (x:xs)
    | x `elem` xs = myNub xs
    | otherwise   = x : myNub xs

-- *Main> isAsc [1,2,3,4,5,6,7]
-- True
-- *Main> isAsc [1,2,4,2]
-- False
isAsc :: [Int] -> Bool
isAsc []  = True
isAsc [x] = True
isAsc (x:y:xs) =
    (x <= y) && isAsc (y:xs)

-- First arg is a func and second is a type
app :: (a -> b) -> a -> b
app f x = f x

-- *Main> app add1 5
-- 6
add1 :: Int -> Int
add1 x = x + 1
