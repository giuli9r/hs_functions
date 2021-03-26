--										Proyecto 1
-- Giuliano Rossetti    38.108.666

-- Ejercicio 1

-- a)
esCero :: Int -> Bool
esCero x = x == 0

-- b)
esPositivo :: Int -> Bool
esPositivo x = x>0

-- c)
esVocal :: Char -> Bool
esVocal c = c `elem` ['a', 'e', 'i', 'o', 'u']

-- Ejercicio 2. Programa usando recursion o composicion.

-- a)
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = (x == True) && paratodo xs

-- b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- c)
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- d)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- e)
promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sumatoria xs) (length xs)


--Ejercicio 3. funcion pertenece

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys


-- Ejercicio 4. funcion encuentra

encuentra:: Int -> [(Int,String)] -> String
encuentra _ [] = ""
encuentra x ((y,s):ss) | x == y = s
                       | otherwise = encuentra x ss



-- Ejercicio 5

-- a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x:xs) t = t x && paratodo' xs t

-- b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] _ = False
existe' (x:xs) t = t x || existe' xs t

-- c)
--sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' :: Num p => [a] -> (a -> p) -> p
sumatoria' [] _ = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t

-- d) 
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) t = t x * sumatoria' xs t


-- Ejercicio 6

paratodo'' :: [a] -> (a -> Bool) -> Bool
paratodo'' xs t = paratodo' xs t      -- El caso recursivo esta definido en paratodo'


-- Ejercicio 7

-- a) 
esPar :: Int -> Bool
esPar x = mod x 2 == 0

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs esPar

-- b)
multiplo :: Int -> Int -> Bool
multiplo y n = (n `mod` y == 0)

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo y xs = existe' xs (multiplo y)

-- c)
sumaCuadrados :: (Num a, Enum a) => a -> a
sumaCuadrados n = sumatoria' [1..n] (^2)

-- d)
factorial' :: Int -> Int
factorial' n = productoria [1..n]

-- e)
multiplicaPares :: [Int] -> Int
multiplicaPares [] = 1
multiplicaPares (x:xs) | esPar x = x * multiplicaPares xs
                       | otherwise = multiplicaPares xs


-- Ejercicio 8

{-
  MAP es una f de orden superior que toma una función (que a su vez ésta toma un a y un b,) 
  y una lista xs y aplica esa función a cada elemento de xs, produciendo una nueva lista.
--------------------------------MAP---------------------------------- >
                                                                   -- >
map :: (a -> b) -> [a] -> [b]                                      -- >
map f [] = []                                                      -- >
map f (x:xs) = f x : map f xs                                      -- >
                                                                   -- >
-------------------------------FILTER-------------------------------- >
                                                                   -- >
filter :: (a -> Bool) -> [a] -> [b]                                -- >
filter p [] = []                                                   -- >
filter p (x:xs) | p x = x : filter p xs                            -- >
                | otherwise = filter p xs                          -- >
                                                                   -- >
--------------------------------------------------------------------- >
  FILTER es una f que toma un predicado y una lista, 
  devolviendo una lista con los elementos que satisfacen el predicado. Es decir, si p x se evalua
  en True, x es incluido a la lista.

-}
{-____________________________________________________________________________________________
 ¿A que equivale la expresion map succ [1, -4, 6, 2, -8], donde succ n = n+1?
  Equivale a la lista [2,-3,7,3,-7], donde cada elemento es el siguiente de la lista original.

 ¿Y la expresion filter esPositivo [1, -4, 6, 2, -8]?
  A la lista de los positivos [1,6,2] pertenecientes a la lista original.
_______________________________________________________________________________________________
-}


-- Ejercicio 9

-- a)
duplicaList :: (Num a) => [a] -> [a]
duplicaList [] = []
duplicaList (x:xs) = (2 * x) : duplicaList xs

-- b)
duplicaList' :: (Num a) => [a] -> [a]
duplicaList' xs = map (*2) xs


-- Ejercicio 10

-- a)
listaPares :: (Integral a) => [a] -> [a]
listaPares [] = []
listaPares (x:xs) | (mod x 2 == 0) = x : listaPares xs
                  | otherwise = listaPares xs

-- b)
listaPares' :: (Integral a) => [a] -> [a]
listaPares' xs = filter even xs

-- c)
-- 7e.
--multiplicaPares :: [Int] -> Int
--multiplicaPares [] = 1
--multiplicaPares (x:xs) | esPar x = x * multiplicaPares xs
--                       | otherwise = multiplicaPares xs

multiplicaPares' :: [Int] -> Int
multiplicaPares' xs =  productoria (listaPares' xs)


-- Ejercicio 11 

--
sumarALista :: Num a => a -> [a] -> [a]
sumarALista _ [] = []
sumarALista y (x:xs) = (y+x) : sumarALista y xs

sumarALista' :: Num b => b -> [b] -> [b]
sumarALista' y xs = map (+y) xs

--
encabezar :: a -> [[a]] -> [[a]]
encabezar _ [] = []
encabezar x [[]] = [[x]]
encabezar x (xs:xss) = (x:xs) : encabezar x xss

encabezar' :: a -> [[a]] -> [[a]]
encabezar' x xss = map (x:) xss

--

mayoresA :: Ord a => a -> [a] -> [a]
mayoresA _ [] = []
mayoresA n (x:xs) | n > x = mayoresA n xs
                  | n == x = (xs)
                  |otherwise = (x:xs)

mayoresA' :: Ord a => a -> [a] -> [a]
mayoresA' n xs = filter (>n) xs


-- Ejercicio 12
{-
encuentra:: Int -> [(Int,String)] -> String
encuentra x [] = ""
encuentra x ((y,s):ss) | (x == y) = s
                       | otherwise = encuentra x ss
					   
					   
-}

igualA :: Int -> (Int, String) -> Bool
igualA n (a,_) = n == a

mostrarS :: [(Int,String)] -> String
mostrarS [] = ""
mostrarS ((_,s):_) = s

encuentra' :: Int -> [(Int, String)] -> String
encuentra' _ [] = ""
encuentra' n ((x,y):xs) = mostrarS (filter (igualA n) ((x,y):xs))

-- Ejercicio 13

-- a)
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA n (x:xs) | n == x = x:(primIgualesA n xs)
                      | otherwise = []

-- b)
primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' n xs = takeWhile (==n) xs


-- Ejercicio 14

-- a)
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:xs) | x == y = x : primIguales (y:xs)
                     | otherwise = [x]

-- b)
primIguales' :: Eq a => [a] -> [a]
primIguales' [] = []
primIguales' (x:xs) = primIgualesA x (x:xs)


-- Ejercicio 15

-- a)
minimo :: Ord a => [a] -> a
minimo [] = error "Lista vacia"
minimo [x] = x
minimo (x:y:xs) | (x < y) = minimo (x:xs)
                | otherwise = minimo (y:xs)

-- b)
minimo' :: Bounded a => Ord a => [a] -> a
minimo' [] = minBound
minimo' [x] = x
minimo' (x:y:xs) | (x < y) = minimo' (x:xs)
                 | otherwise = minimo' (y:xs)
