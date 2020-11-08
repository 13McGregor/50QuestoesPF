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

--14
myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits l = myInits (myInit l) ++ [l]

myInit :: [a] -> [a]
myInit [a] = []
myInit (h:t) = h : myInit (t)

--15
myTails :: [a] -> [[a]]
myTails [] = [[]]
myTails (h:t) = [(h:t)] ++ myTails (t)

--16
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf (h1:t1) (h2:t2) | h1 /= h2 = False
                             | h1 == h2 = myIsPrefixOf t1 t2

--17                             
myIsSuffixOf :: Eq a => [a] -> [a] -> Bool
myIsSuffixOf [] _ = True
myIsSuffixOf l1 l2 | last l1 /= last l2 = False
                   | last l1 == last l2 = myIsSuffixOf (myInit l1) (myInit l2)

--18
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myIsSubsequenceOf [] _ = True
myIsSubsequenceOf _ [] = False
myIsSubsequenceOf (h1:t1) (h2:t2) | h1 == h2 = myIsSubsequenceOf t1 t2
                                  | h1 /= h2 = myIsSubsequenceOf (h1:t1) t2

--19
myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices a [] = []
myElemIndices a l = aux19 0 a l

aux19 :: Eq a => Int -> a -> [a] -> [Int]
aux19 _ _ [] = []
aux19 n i (h:t) | i == h = n : (aux19 (n+1) i t) 
                | i /= h = aux19 (n+1) i t 

--20
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub [a] = [a]
myNub (h:t) = h : myNub (aux20 h t)

aux20 :: Eq a => a -> [a] -> [a]
aux20 a [] = []
aux20 a (h:t) | a == h = aux20 a t
              | otherwise = h : aux20 a t

--21
myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete a (h:t) | a == h = t
                 | otherwise = h : myDelete a t

--22
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) [] _ = []
(\\\) l [] = l
(\\\) (h1:t1) (h2:t2) | h2 == h1 = (\\\) t1 t2
                      | otherwise = h1 : (\\\) t1 (h2:t2)

--23
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion l [] = l
myUnion l (h:t) | myElem h l = myUnion l t
                | otherwise = myUnion (l++[h]) t

--24
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect (h:t) l | myElem h l = h : myIntersect t l
                    | otherwise = myIntersect t l

--25
myInsert :: Ord a => a -> [a] -> [a]
myInsert a [] = [a]
myInsert a (h:t) | a <= h = a : (h:t)
                 | otherwise = h : myInsert a t

--26
myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords [s] = s
myUnwords (h:t) = h ++ " " ++ myUnwords t

--27
myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines (h:t) = h ++ "/n" ++ myUnlines t

--28
