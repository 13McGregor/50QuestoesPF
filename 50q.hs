-- 1
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo a b | a > b = []
                 | a == b = [a]
                 | otherwise = a:myEnumFromTo (a+1) b

--2
myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo a b c | a > c && a < b = []
                       | a < c && a > b = [] 
                       | a < c =  a : myEnumFromThenTo b (b+(b-a)) c
                       | a > c =  a : myEnumFromThenTo b (b-(a-b)) c

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
myTake 0 (h:t) = t
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
myPMaior ::  Ord a => [a] -> Int
myPMaior (h:t) = pos (maior h t) (h:t)

maior :: Ord a => a -> [a] -> a
maior a [] = a
maior a (h:t) | a > h = maior a t
              | otherwise = maior h t

pos :: Ord a => a -> [a] -> Int
pos _ [] = 0
pos a (h:t) | a == h = 0
            | otherwise = 1 + pos a t

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos (h:t) = aux29 h t


aux29 :: Eq a => a -> [a] -> Bool
aux29 _ [] = False
aux29 a (h:t) | a == h = True
              | otherwise = aux29 a t

--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) | (h >= '0' && h <= '9') = h : algarismos t
                 | otherwise = algarismos t

--31
posImpares ::  [a] -> [a]
posImpares [] = []
posImpares (h:i:t) = i : posImpares t

--32
posPares ::  [a] -> [a]
posPares [] = []
posPares (h:i:t) = h : posPares t

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (h:i:t) | h <= i = isSorted (i:t)
                 | otherwise = False

--34
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

--44
data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:t) = posicao (x, y-1) t
posicao (x,y) (Sul:t) = posicao (x, y+1) t
posicao (x,y) (Este:t) = posicao (x+1, y) t
posicao (x,y) (Oeste:t) = posicao (x-1, y) t

--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (c,d) | a==c && b==d = []
                    | a < c = Este : caminho (a+1,b) (c,d)
                    | a > c = Oeste : caminho (a-1,b) (c,d)
                    | b < d = Sul : caminho (a,b+1) (c,d)
                    | b > d = Norte : caminho (a,b-1) (c,d)

--46
vertical ::  [Movimento] -> Bool
vertical [] = True
vertical (Norte:t) = vertical t
vertical (Sul:t) = vertical t
vertical (Este:t) = False
vertical (Oeste:t) = False

--47
data Posicao = Pos Int Int
                deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [Pos a b] = Pos a b 
maisCentral (Pos a b : Pos c d : t) | ( a^2 + b^2 ) < ( c^2 + d^2 ) = maisCentral (Pos a b : t)
                                            | otherwise = maisCentral (Pos a b : t)

--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) (Pos w z : t) | (w == x + 1) && (z >= y - 1) && (z <= y + 1) = (Pos w z) : vizinhos (Pos x y) t
                                 | (w == x - 1) && (z >= y - 1) && (z <= y + 1) = (Pos w z) : vizinhos (Pos x y) t
                                 | (w == x) && ((z == y - 1) || (z == y + 1))   = (Pos w z) : vizinhos (Pos x y) t
                                 | otherwise = vizinhos (Pos x y) t

--49 
mesmaOrdenada ::  [Posicao] -> Bool
mesmaOrdenada [Pos a b] = True
mesmaOrdenada (Pos a b : Pos c d : t) | b == d = mesmaOrdenada (Pos c d : t)
                                      | otherwise = False

--50
data Semaforo = Verde | Amarelo | Vermelho
              deriving Show

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK l | nVermelhos l < 2 = True
                | otherwise = False

nVermelhos :: [Semaforo] -> Int
nVermelhos [] = 0
nVermelhos (Verde:t) = 1 + nVermelhos t
nVermelhos (Amarelo:t) = 1 + nVermelhos t
nVermelhos (Vermelho:t) = nVermelhos t