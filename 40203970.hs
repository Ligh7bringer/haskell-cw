{-
READ ME: Change the name of this file to YOURSTUDENTNUMBER.hs. For example, if your
student number were 123456789, then you would rename the file 123456789.hs.

Fill in the function definitions for each of the questions, and uncomment them. 
The names of the functions correspond to the names given in the document cwk_15_16.pdf. 

DO NOT CHANGE THE TYPE SIGNATURES, OR THE ORDER OF THE ARGUMENTS!

You may add as many extra or helper functions as you wish, but do not make any "import" statements.
-}

-- QUESTION 1 
isReflexive :: (Eq a) => [(a,a)] -> Bool
isReflexive [] = True
isReflexive xs
	| subset (convertList(reduceList xs)) xs = True
	| otherwise = False

isSymmetric :: (Eq a) => [(a,a)] -> Bool
isSymmetric xs = and [ (y,x) `elem` xs | (x,y) <- xs ]


isTransitive :: (Eq a) => [(a,a)] -> Bool
isTransitive xs = and [ (x,z) `elem` xs | (x,y) <- xs, (y',z) <- xs, y==y' ]


isEquivalence :: (Eq a) => [(a,a)] -> Bool
isEquivalence xs = isSymmetric xs && isReflexive xs && isTransitive xs


eqClassOf :: (Eq a) => [(a,a)] -> a -> [a]
eqClassOf xs x = (eqClassOf' xs x)


eqClassOf' :: (Eq a) => [(a,a)] -> a -> [a]
eqClassOf' [] y = []
eqClassOf' (x:xs) y 
	| y == (fst x) = snd x : eqClassOf' xs y
	| otherwise =  eqClassOf' xs y


-- TEST SET FOR Q1
{-
Your functions should have the following behaviour:
isReflexive [(1,2),(2,1),(1,1),(2,2)] is True 
isReflexive [(1,2),(2,1),(2,2)] is False
isSymmetric [(1,2),(2,1),(1,1),(2,2)] is True
isSymmetric [(1,2),(1,1),(2,2)] is False
isTransitive [(1,2),(2,1),(1,1),(2,2)] is True
isTransitive [(1,2),(2,3)] is False
isEquivalence [(1,2),(2,1),(1,1),(2,2)] is True
eqClassOf [(1,2),(2,1),(1,1),(2,2)] 1 is [1,2]
-}

-- QUESTION 2


multiEqual :: (Eq a) => [a] -> [a] -> Bool
multiEqual xs ys 
	| length xs /= length ys = False
	| otherwise = and [count x xs == count x ys | x <- xs]

multiUnion :: (Eq a) => [a] -> [a] -> [a]
multiUnion xs ys = setUnique1 (xs ++ ys)


multiIntersection :: (Eq a) => [a] -> [a] -> [a]
multiIntersection xs ys = setUnique2 ( xs ++ ys)

-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:
multiEqual [1,1,2] [1,2,1] is True
multiEqual [1,1,2] [1,2] is False
multiUnion [1,1,2] [1,2,2] is [1,1,2,2] 	
multiIntersection [1,1,2] [1,2,2] is [1,2]
-}

-- QUESTION 3
trace :: (Num a) => [[a]] -> Maybe a
trace xs 
	| fst (matrixSize xs) == snd (matrixSize xs) = Just (sum (getDiagonal xs))
	| otherwise = Nothing

matMult3 :: (Num a) => [[a]] -> [[a]] -> [[a]]
matMult3 xs ys = convert ( [foldl (+) 0 $ zipWith (*) x y | x <- xs, y <- transpose' ys])




-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
trace [[1,2],[6,4]] is Just 5
matMult3 [[1,0,1],[0,1,0],[0,0,1]] [[0,1,0],[1,0,1],[1,1,0]] is
[[1,2,0],[1,0,1],[1,1,0]]
-}

-- QUESTION 4
--FIRST ARGUMENT IS ROW NUMBER, SECOND IS SEED/VALUE AT TIP OF TRIANGLE
triNumber :: Int -> Int -> [Int]
triNumber n k = map (triangle k n) [1..n]

-- TEST SET FOR Q4
{-
Your function should have the following behaviour:
triNumber 3 1 is [2,3,5]
-}

-- QUESTION 5
combine :: Int -> Int -> (Int, Int, Int)
combine a b = euc a b

-- TEST SET FOR Q5
{-
Your function should have the following behaviour:
combine 3 2 is (1,-1,1)
-}



--HELPER FUNCTIONS

removeDup [] = []
removeDup (x:xs)
	| elem x xs = removeDup xs
	| otherwise = x : removeDup xs  

reduceList xs = uniqueList (allFst xs ++ allSnd xs) 

--convertList :: (Eq a) => [a] -> [(a,b)] 
convertList [] = []
convertList (x:xs) = (x,x) : convertList xs 

subset :: (Eq a) => [a] -> [a] -> Bool
subset [] _ = True
subset (x:xs) ys
	| elem x ys = subset xs ys 
	| otherwise = False

uniqueList :: (Eq a) => [a] -> [a]
uniqueList [] = []
uniqueList (x:xs)
	| elem x xs = uniqueList xs
	| otherwise = x : uniqueList xs

allFst :: [(a,b)] -> [a]
allFst [] =  []
allFst (x:xs) = fst x : allFst xs

allSnd :: [(a,b)] -> [b]
allSnd [] =  []
allSnd (x:xs) = snd x : allSnd xs

count elem xs = length $ filter (\x -> x == elem) xs

setUnique1 :: (Eq a) => [a] -> [a]
setUnique1 [] = []
setUnique1 (x:xs)
    | elem x xs = x : setUnique1 xs
    | otherwise =  setUnique1 xs


setUnique2 :: (Eq a) => [a] -> [a]
setUnique2 [] = []
setUnique2 (x:xs)
    | elem x xs = setUnique2 xs
    | otherwise = x : setUnique2 xs

matrixSize :: [[a]] -> (Int,Int)
matrixSize (x:xs)
	| isMatrix xs = (length xs + 1, length x) 
	| otherwise = (0,0)

isMatrix :: [[a]] -> Bool
isMatrix xs
	| length (uniqueList (map (length) xs)) == 1 = True
	| otherwise = False

getDiagonal :: (Num a) => [[a]] -> [a]
getDiagonal [[]] = []
getDiagonal (xs:[]) = [head xs]
getDiagonal (x:xs) = head x : getDiagonal (map tail xs)

transpose' :: [[a]] -> [[a]]
transpose' ys
   | null $ filter (not . null) ys = []
   | otherwise = [flatten' $ map head' ys] ++ transpose' (map tail' ys)

head' :: [a] -> [a]
head' []     = []
head' (x:xs) = [x]

tail' :: [a] -> [a]
tail' []     = []
tail' (x:xs) = xs

flatten' :: [[a]] -> [a]
flatten' as = foldl (\acc x -> acc ++ x)  [] as

convert [a,b,c,d,e,f,g,h,i] = [[a,b,c],[d,e,f],[g,h,i]]

triangle k 0 0 = k
triangle k y 1 = triangle k (y - 1) (y - 1) 
triangle k y x = triangle k (y - 1) (x - 1) + triangle k y (x - 1) 


euc :: (Integral a) => a -> a -> (a, a, a)
euc a b = case b of
            1 -> (0, 1, 1)
            _ -> let (e, f, u) = euc b d
                 in (f, e - c*f, u)
  where c = a `div` b
        d = a `mod` b
        u = a `mod` b