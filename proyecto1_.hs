-- 2019								Proyecto 1
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
esVocal c = elem (c) (['a', 'e', 'i', 'o', 'u'])

-- Ejercicio 2. Programa usando recursion o composicion.

-- a)
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) | x == True = paratodo xs
                | otherwise = False

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
pertenece x [] = False
pertenece x (y:ys) | (x == y) = True
                   | otherwise = pertenece x ys


-- Ejercicio 4. funcion encuentra

encuentra:: Int -> [(Int,String)] -> String
encuentra x [] = ""
encuentra x ((y,s):ss) | (x == y) = s
                       | otherwise = encuentra x ss
 


-- Ejercicio 5

-- a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = (t x) && (paratodo' xs t)

-- b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = (t x) || existe' xs t

-- c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = (t x) + sumatoria' xs t

-- d) 
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = (t x) * sumatoria' xs t 


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
sumaCuadrados :: Int -> Int
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
sumarALista y [] = []
sumarALista y (x:xs) = (y+x) : sumarALista y xs

sumarALista' y xs = map (+y) xs

--
encabezar :: a -> [[a]] -> [[a]]
encabezar x [] = []
encabezar x [[]] = [[x]]
encabezar x (xs:xss) = (x:xs) : encabezar x xss

encabezar' x xss = map (x:) xss

--

mayoresA :: Ord a => a -> [a] -> [a]
mayoresA n [] = []
mayoresA n (x:xs) | n > x = mayoresA n xs
                  | n == x = (xs)
                  |otherwise = (x:xs)

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
mostrarS ((x,s):ss) = s

encuentra' :: Int -> [(Int, String)] -> String
encuentra' n [] = ""
encuentra' n ((x,y):xs) = mostrarS (filter (igualA n) ((x,y):xs))

-- Ejercicio 13

-- a)
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) | n == x = x:(primIgualesA n xs)
                      | otherwise = []

-- b)
primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' n xs = takeWhile (==n) xs


-- Ejercicio 14

-- a)
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
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



-- Ejercicio 16*

{-

 a)
 f :: (a, b) -> ... 
 f x = ...

El tipado es correcto. Cuando se hace pattern matching con 'f x', x se comporta como una tupla, y para utilizar los elementos a y b se usan las funciones fst y snd.
cubre todos los casos.

 b) 
 f :: (a, b) -> ...
 f (x , y) = ...

Correctamente tipado. En la definicion de f se puede hacer pattern matching con los elementos de la tupla. Cubre todos los casos.



 c) 
 f :: [(a, b)] -> ...
 f (a , b) = ...

Tipado incorrecto. El primer parametro a ingresar en f es una lista (de tuplas) pero en su definicion se espera una tupla solo.

Tipado correcto:
     f :: (a,b) -> ...
     f (a,b) = ...	 


 d) 
 f :: [(a, b)] -> ...
 f (x:xs) = ...

Tipado correcto. El tipado :: indica que f es una lista de tuplas (a,b) y en su definicion f espera como primer parametro una lista de elementos x, que son las tuplas.
No cubre el caso de listas vacias, f [] = ...
Para utilizar a y b por separados utilizar las funciones fst y snd.

 e) 
 f :: [(a, b)] -> ...
 f ((x, y) : ((a, b) : xs)) = ...

Correctamente tipado. la funcion f tiene tipo (::) lista de tuplas, y en su definicion se hace explicito el primer par de tuplas del parametro a ingresar.
Los casos que no cubre son cuando la lista es vacia o de un solo elemento. ademas tiene parentesis de más, ya que ':' es asociativo a derecha, esto es:

        f ((x,y) : ((a,b) : xs ))  =   f ( (x,y) : (a,b) : xs )


 f)
 f :: [(Int, a)] -> ...
 f [(0, a)] = ...

Correcto tipado. f :: lista de tuplas, donde el primer elemento del par es un entero (Int) y el segundo un elemento de tipo a.
En la deficion de f cubre solo aquellas listas donde el primer elemento del par es 0, sin cubrir listas vacias.


 g) 
 f :: [(Int, a)] -> ...
 f ((x, 1) : xs) = ...

Correctamente tipado. f tiene tipo lista de tuplas, y en su definicion ((x,1):xs) es una lista de tuplas del tipo (x,1) donde x tiene tipo Int. 
No cubre listas vacias ni aquellas donde el segundo elemento del par sea distinto de 1.


 h)
 f :: [(Int, a)] -> ...
 f ((1, x) : xs) = ...

Idem anterior. Excepto que el par tiene un 1 (Int) como primer elemento y un x de tipo a como segundo.


 i)
 f :: (Int -> Int) -> Int -> ...
 f a b = ...

f toma como primer parametro una funcion del tipo (Int -> Int) y un segundo paramentro del mismo tipo. En su  definicion 'a' ocupa el lugar de la funcion a ingresar (del tipo Int -> Int)
y 'b' es el segundo parametro a ingresar del tipo Int. Luego esta bien tipado.


 j)
 f :: (Int -> Int) -> Int -> ...
 f a 3 = ...
 
Idem anterior. Excepto que en la definicion el segundo parametro es una constante 3. Es decir, f cubre los casos donde 'b' (en i) es igual a 3 y no cubre ningun otro caso. 
 
 
 k)
 f :: (Int -> Int) -> Int -> ...
 f 0 1 2 = ...

Tipado incorrecto. f toma una funcion como primer parametro del tipo (Int -> Int) y luego toma otro parametro del mismo tipo.
En su definicion, f toma parametros que son iguales a 0 1 2 , es decir, del tipo Int -> Int -> Int. La utilizacion de parentesis hace explicita la asociasion por derecha de ->

Correcion de tipado:

f :: Int -> Int -> Int -> ...
f 0 1 2 = ...

No cubre ningun caso donde los parametros sean distintos a 0, 1 o 2, en dicho orden.


 l)
 f :: a -> (a -> a) -> ...
 f a g = ...

Correctamente tipado.
-}


-- Ejercicio 17

{-

 a) f :: (a, b) -> a
 definicion de first (fst)
 
 b) f :: (a, b) -> b
 definicion de second (snd)
 
 c) f :: (a, b) -> c
    f (a,b) = a+b
 
 d) f :: a -> b            (con restriccion Integer a, Bool b)
    f a = (a `mod` 2 == 0)
 
 e) f :: (a -> b) -> a -> b
-}


