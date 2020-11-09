--34
myInsert :: Ord a => a -> [a] -> [a]
myInsert a [] = [a]
myInsert a (h:t) | a <= h = a : (h:t)
                 | otherwise = h : myInsert a t

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = myInsert h (iSort t)

--35
menor :: String -> String -> Bool
menor [] [] = False
menor [] _ = True
menor _ [] = False
menor (h1:t1) (h2:t2) | h1 == h2 = menor t1 t2
                      | h1 > h2 = False
                      | h1 < h2 = True

--36
elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((x,y):t) | a == x = True
                     | otherwise = elemMSet a t

--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):t) = y + lengthMSet t

--38
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate a b = b: myReplicate (a-1) b

converteMSet ::  [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):t) = myReplicate y x ++ converteMSet t

--39
insereMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t) | a == x = ((x,y+1):t)
                       | otherwise = (x,y) : insereMSet a t

--40
removeMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a ((x,y):t) | a == x && y == 1 = removeMSet a t
                       | a == x = ((x,y-1):t)
                       | otherwise = (x,y) : removeMSet a t

--41
contaIguais :: Eq a => [a] -> Int
contaIguais [] = 0
contaIguais [a] = 1
contaIguais (h:i:t) | h == i = 1 + contaIguais (i:t)
                    | otherwise = 1

constroiMSet ::  Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = (( h , contaIguais (h:t))) : constroiMSet (drop(contaIguais (h:t)) (h:t))

--42
partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers l = (lefts l, rights l)

lefts :: [Either a b] -> [a]
lefts [] = []
lefts (Left a : t) = a : lefts t
lefts (Right b : t) = lefts t

rights :: [Either a b] -> [b]
rights [] = []
rights (Left a : t) = rights t
rights (Right b : t) = b : rights t

--43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just h : t) = h : catMaybes t
catMaybes (Nothing : t) = catMaybes t