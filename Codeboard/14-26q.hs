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
myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (h:t) | a == h = True
               | otherwise = myElem a t

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