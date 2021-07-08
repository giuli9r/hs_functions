--				Proyecto 2
--		      Tipos de datos										   Giuliano Rossetti
--																       38.108.666



{- Ejercicio 1 - Tipos Enumerados -}

data Carrera = Matematica | Fisica | Computacion | Astronomia   deriving (Eq, Ord, Show)

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"



{- Ejercicio 2 - Clases de Tipos -}

-- Se agrega el tipo Carrera a las clases Eq (provee una nocion de comparacion), 
-- Ord (provee una nocion de orden) y Show (para la representacion en forma de texto).



{- Ejercicio 3 - Tipos enumerados; constructores con parametros -}


type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar   deriving (Eq, Show)
data Area = Administrativa | Enseñanza | Economica | Postgrado     deriving (Eq, Show)

data Persona = Decane
            | Docente Cargo
            | NoDocente Area
            | Estudiante Carrera Ingreso    deriving (Eq, Show)

-- b) El tipo del constructor Docente es Cargo -> Persona.

-- c)
cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (per:ps) c | (per == Docente c) = 1 + cuantos_doc ps c
                       | otherwise = cuantos_doc ps c

{-
cuantos_doc'' :: [Persona] -> Cargo -> Int
cuantos_doc'' ((Docente c):ps) c = 1 + cuantos_doc'' ps c           << No Eq. 
cuantos_doc'' (_:ps) c = cuantos_doc'' ps c                         << Error: Conflicting defs.
cuantos_doc'' [] c = 0
-}

-- d)
cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' ps c = length (filter (== Docente c) ps)



{- Ejercicio 4 - Tipos enumerados con polimorfismo -}


-- a)
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x



{- Ejercicio 5 - Tipos recursivos -}


data Cola = VaciaC | Encolada Persona Cola

-- a)
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada p ps) = Just ps

encolar :: Persona -> Cola -> Cola
encolar per VaciaC = Encolada per VaciaC
encolar per (Encolada per2 ps) = Encolada per2 (encolar per ps)

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC cargo = Nothing
busca (Encolada per cola) cargo | (per == Docente cargo) = Just per
                                | otherwise = busca cola cargo


-- b) Al tipo de Listas. data Lista [a] = [] | : a (Lista [a])  <- bien definido?



{- Ejercicio 6 - Tipos recursivos y polimorficos -}


data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b )   deriving(Eq, Show)

-- a)
type GuiaTel = ListaAsoc String Int

-- b)
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b ls) = 1 + la_long ls

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat (la) Vacia = la
la_concat Vacia (la) = la
la_concat (Nodo a b la) (la') = Nodo a b (la_concat (la) la') 

la_pares :: ListaAsoc a b -> [(a,b)]
la_pares Vacia = []
la_pares (Nodo a b la) = (a,b) : la_pares la

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia clave = Nothing
la_busca (Nodo a b la) clave | a == clave = Just b
                             | otherwise = la_busca la clave


la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b 
la_borrar clave Vacia = Vacia
la_borrar clave (Nodo a b ls) | a == clave = ls
                                | otherwise = la_concat (Nodo a b Vacia) (la_borrar clave ls)



{- Ejercicio 7 - Arbol. Punto * -}


data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)   deriving (Eq, Show, Ord)

type Prefijos = Arbol String

can , cana , canario , canas , cant , cantar , canto :: Prefijos

can = Rama cana "can" cant
cana = Rama canario "a" canas
canario = Rama Hoja "rio" Hoja
canas = Rama Hoja "s" Hoja
cant = Rama cantar "t" canto
cantar = Rama Hoja "ar" Hoja
canto = Rama Hoja "o" Hoja

-- a)

a_long :: Arbol a -> Int
a_long (Rama ar_1 dato ar_2) = 1 + a_long ar_1 + a_long ar_2
a_long Hoja = 0

-- b)

a_hojas :: Arbol a -> Int
a_hojas Hoja = 0                                                       -- <Funcion correcta, si se define a la hoja como valor concreto final de un arbol, sin subarboles (hijos)> Uno podria pensar que el constructor Hoja equivale a un vacio, por ende su valor es 0. Aquel dato final con 2 Hojas como "hijos" (subarboles) necesita contarse una sola vez ya que vendria a ser una hoja de por si.
a_hojas (Rama Hoja _ Hoja) = 1
a_hojas (Rama ar_1 dato ar_2) = a_hojas ar_1 + a_hojas ar_2

{-
a_hojas' :: Arbol a -> Int
a_hojas' Hoja = 1                                                       <Funcion incorrecta> Se le da equivocamente un valor de 1 al constructor Hoja; de esta forma cuenta dos hojas por dato sin subarbol y el valor de hojas es el doble del correcto.
a_hojas' (Rama ar_1 dato ar_2) = a_hojas' ar_1 + a_hojas' ar_2
-}

-- c)

a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama ar_1 n ar_2) = Rama (a_inc ar_1) (n+1) (a_inc ar_2)

-- d)

a_map :: (a -> b) -> Arbol a -> Arbol b
a_map f Hoja = Hoja
a_map f (Rama ar_1 dato ar_2) = Rama ( a_map f ar_1) (f dato) (a_map f ar_2)











