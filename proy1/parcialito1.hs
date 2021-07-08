{- Giuliano Rossetti   DNI 38.108.666	TEMA A -}


-- Ejercicio 1

estaEnDNI :: Int -> Bool
estaEnDNI n = n==3 || n ==8 || n==1 || n ==0 || n==6


--Ejercicio 2

sumaDNI :: [Int] -> Int
sumaDNI [] = 0
sumaDNI (x:xs) | estaEnDNI x = x + sumaDNI xs
               | otherwise = sumaDNI xs


--Ejercicio 3

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t

estaEnDNI' :: Int -> Int
estaEnDNI' n | (n==3 || n ==8 || n==1 || n==6) = n
             | otherwise = 0

sumaDNI' :: [Int] -> Int
sumaDNI' [] = 0
sumaDNI' xs = sumatoria' xs estaEnDNI'


--Ejercicio 4

reducir :: [a] -> (a -> a -> a) -> a
reducir [x] op = x
reducir (x:y:xs) op = op ((op) x y) (reducir xs op)

{- 
   La funcion reducir ejecuta solo para listas con cantidad de elementos impares, ya que no tiene en cuenta el caso de listas vacias.
   Si la funcion es tratada recursivamente, es inevitable (en algunos casos) tratar con listas vacias.
   Una forma de resolver esto es  devolver el elemento neutro del operador en cuestion en el caso de lista vacia.
   Otra manera, verificar la longitud de la lista y agregar el elemento neutro a la misma cuando haga falta.
   Este problema sucede (creo yo), porque va "tomando" de 2 en 2 los elementos de la lista, si la cantidad de elementos es impar
   entonces por pattern matching [x] -> x, nos devuelve el ultimo elemento. Si la cant de elem. es par, estamos frente al caso
   de lista vacia *reducir [] op* y nos devuelve el error  Non-exhaustive patterns in function reducir.
-}

reducir2 :: [a] -> (a -> a -> a) -> a
reducir2 [x] op = x
reducir2 (x:xs) op = op x (op (reducir2 xs op))

reducir' :: [a] -> (a -> a -> a) -> a
reducir' [x] op = x
reducir' xs op = map xs op
