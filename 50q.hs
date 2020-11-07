-- 1
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo a b | a > b = []
                 | a == b = [a]
                 | otherwise = a:myEnumFromTo (a+1) b

--2

myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo a b c | a > c && a < b = []
                       | a < c && a > b = [] 
                       | otherwise =  a : myEnumFromThenTo b (b+(b-a)) c


--3
myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus [] l = l
myPlusPlus l [] = l
myPlusPlus (h:t) l = h: myPlusPlus t l

--4
(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) a = (!!!) t (a-1)

--5
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myreverse t ++ [h]

--6
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake a (h:t) = h : myTake (a-1) t

--7
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 l = l
myDrop a (h:t) = myDrop (a-1) t

--8
myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (h1:t1) (h2:t2) = (h1,h2): myZip t1 t2

--9
myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (h:t) | a == h = True
               | otherwise = myElem a t

--10
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate a b = b: myReplicate (a-1) b

--11
myIntersperce :: a -> [a] -> [a]
myIntersperce a [] = []
myIntersperce a (h:[]) = [h]
myIntersperce a (h:t) = h : a : myIntersperce a t

--12
contaIguais :: Eq a => [a] -> Int
contaIguais [] = 0
contaIguais [a] = 1
contaIguais (h:i:t) | h == i = 1 + contaIguais (i:t)
                    | otherwise = 1

myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup l = (take(contaIguais l) l) : myGroup(drop(contaIguais l) l)

--13
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (h:t) = h ++ (myConcat t)