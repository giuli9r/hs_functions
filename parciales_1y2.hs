--  Giuliano Rossetti
--  Parcial 1
--  Resolucion de Parcial 1

esMultiplo:: (Int,Int) -> Bool
esMultiplo (a,b) = mod a b == 0 || mod b a == 0

dividir :: [(Int,Int)] -> [(Int,Int)]
dividir [] = []
dividir ((x,y):zs) = (div x y, mod x y):dividir zs

hacerA :: String -> String
hacerA [] = []
hacerA (x:xs) | x == 'e' || x == 'i' || x == 'o' || x == 'u' = 'a':hacerA xs
              | otherwise = x:hacerA xs

palabraMacabra :: String -> Bool
palabraMacabra xs = hacerA xs == xs



--  Parcial 2
--  Resolucion de Parcial 2. 


import Data.List (nub)

{- Ejercicio 1 -}

data Negocio = Supermercado | Farmacia | Panaderia | Verduleria   deriving (Show)

type Items = [String]

pedido :: Negocio -> Items
pedido Supermercado = ["gaseosas", "carne", "te"]
pedido Farmacia = ["alcohol en gel", "vitaminas"]
pedido Panaderia = ["pan", "criollos"]
pedido Verduleria = ["cebolla", "papa"]


{- Ejercicio 2 -}

data Compra = Ninguna | AgregarCompra Negocio Items Compra   deriving (Show)

cuantos_items :: Compra -> Int
cuantos_items Ninguna = 0
cuantos_items (AgregarCompra neg it com) = length (nub (it)) + cuantos_items com    -- <En el caso de haber anotado dos veces un mismo item, se cuenta solo una.


{- Ejercicio 3 -}

-- Definicion de tipos Persona, Rol
data Funcion = Teorico | Practico 
     deriving (Eq, Show)

data Rol = Decanx
           | Docente Funcion
           | Estudiante Carrera Ingreso
    deriving (Eq, Show)

data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado
    deriving (Eq, Show)

data Persona = Per String Int Int Rol
    deriving (Eq, Show)

type Ingreso = Int


busca_est_dni :: [Persona] -> Int -> String
busca_est_dni [] dnibuscar = ""
busca_est_dni ( (Per name dni nac (Estudiante c i)) : ps ) dnibuscar | dnibuscar == dni = name
                                                                     | otherwise = busca_est_dni ps dnibuscar
busca_est_dni ( p : ps) dnibuscar =										           busca_est_dni ps dnibuscar


{- Ejercicio 4 -}

data ListaAsoc a b = LVacia | Nodo a b (ListaAsoc a b)   deriving (Show, Read)


{- Funiciones auxiliares -}
la_to_list :: ListaAsoc a b -> [ListaAsoc a b]
la_to_list (Nodo a b LVacia) = [Nodo a b LVacia]
la_to_list (Nodo a b guia) = (Nodo a b LVacia) : la_to_list guia

nodo_to_a :: [ListaAsoc a b] -> [a]
nodo_to_a [] = []
nodo_to_a ((Nodo a b _) : guia) = a : nodo_to_a guia

-- minimo :: Ord a => [a] -> a. Funcion definida en proyecto 1, Ejercicio 15. 
minimo :: Ord a => [a] -> a
minimo [] = error "Lista vacia"
minimo [x] = x
minimo (x:y:xs) | (x < y) = minimo (x:xs)
                | otherwise = minimo (y:xs)

{- Funiciones auxiliares -}
la_min_clave :: Ord a => ListaAsoc a b -> a
la_min_clave LVacia = error "Lista de Asociaciones vacia."
la_min_clave (la) = minimo (nodo_to_a (la_to_list la))













