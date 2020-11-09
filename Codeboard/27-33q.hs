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