--				Proyecto 1
--		Funciones, tipos y alto orden							Giuliano Rossetti
--																       38.108.666

{- Ejercicio 1 -}

esCero :: Int -> Bool
esCero n = n == 0

esPositivo :: Int -> Bool
esPositivo n = n > 0

esVocal :: Char -> Bool
esVocal v = (v == 'a') || (v == 'e') || (v == 'i') || (v == 'o') || (v == 'u')


{- Ejercicio 2 -}

paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = (x == True) && paratodo xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 1
productoria (z:zs) = z * productoria zs

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

promedio :: [Int] -> Int
promedio (x:xs) = div (sumatoria (x:xs)) (length (x:xs))


{- Ejercicio 3 -}

pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = (x == n) || pertenece n xs


{- Ejercicio 4 -}

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = t x && paratodo' xs t

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = t x || existe' xs t

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = t x * productoria' xs t


{- Ejercicio 5 -}

paratodo'' (x:xs) = paratodo' (xs) (==True)


{- Ejercicio 6 -}

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs even

multiplo :: Int -> Int -> Bool           -- Funcion auxiliar para hayMultiplo
multiplo y x = (x `mod` y == 0)

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (multiplo n)

soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | multiplo 2 x = x : soloPares xs
                 | otherwise = soloPares xs

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [1..n] (^2)

factorial' :: Int -> Int
factorial' n = productoria' [1..n] id

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' (soloPares xs) id



{- Ejercicio 7 -}

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


{- Ejercicio 8 -}

duplicaLista :: [Int] -> [Int]
duplicaLista [] = []
duplicaLista (x:xs) = 2*x : duplicaLista xs

duplicaLista' :: [Int] -> [Int]
duplicaLista' xs = map (*2) xs


{- Ejercicio 9 -}

listaPar :: [Int] -> [Int]
listaPar [] = []
listaPar (x:xs) | even x = x : listaPar xs
                | otherwise = listaPar xs


listaPar' :: [Int] -> [Int]
listaPar' xs = filter even xs         -- la funcion even toma un numero y devuelve un Booleano. True <- Par; False <- Impar.

{-
multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' (soloPares xs) id
-}

multiplicaPares' :: [Int] -> Int
multiplicaPares' xs = productoria (filter even xs)


{- Ejercicio 10 -}

primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) | n == x = n : primIgualesA n xs
                      | otherwise = []

primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' n xs = takeWhile (==n) xs


{- Ejercicio 11 -}

primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:ys) | (x == y) = x : primIguales (y:ys) 
                     | otherwise = [x]

primIguales' :: Eq a => [a] -> [a]
primIguales' (x:xs) = primIgualesA' x (x:xs)


{- Ejercicio 12* -}

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
     f :: [(a,b)] -> ...
     f ((a,b):ls) = ...	 



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

No es correcto el tipado porque no se puede inferir el tipo de 1, ya que es ambiguo.



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


{- Ejercicio 13* -}

{-

 a) f :: (a, b) -> b
 definicion de second (snd)
 
 b) f :: (a, b) -> c
    f (a,b) = a+b
 
 c) f :: a -> b
    f x = round x
 
-}











