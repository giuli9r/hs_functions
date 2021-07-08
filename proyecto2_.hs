-- 2019								                 Proyecto 2


-- Ejercicio 1

--a) Tipos enumerados

data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado   -- Para definir la funcion titulo usando analisis por casos, el tipo Carreras debe ser parte de la clase de tipos Eq
    deriving (Eq, Show)

-- b)

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"
titulo Profesorado = "Profesor"


-- c)
{-
 No se puede definir la funcion "titulo" usando analisis por casos porque el tipo Carrera no pertenece
 a la clase de tipos Eq, por ende no acepta comparacion (== o /=). Para salvar este error es necesario
 derivar a Carrera en Eq. A menos que se use la sintaxis case of y pattern matching.

titulo'' :: Carrera -> String
titulo'' t = case t of Matematica -> "Licenciatura en Matematica"
                       Fisica -> "Licenciatura en Fisica"
					 (..)
-}

titulo' :: Carrera -> String
titulo' t | t == Matematica = "Licenciatura en Matematica"                      -- funcion titulo usando  analisis por casos luego de derivar Carreras en typeclass Eq.
          | t == Fisica = "Licenciatura en Fisica"
          | t == Computacion = "Licenciatura en Ciencias de la Computacion"
          | t == Astronomia = "Licenciatura en Astronomia"
          | t == Profesorado = "Profesor"

-- Ejercicio 2

-- a)

type Ingreso = Int

data Funcion = Teorico | Practico 
     deriving (Eq, Show)

data Rol = Decanx Genero                                         -- constructor sin argumento
           | Docente Funcion                                     -- constructor con un argumento
           | Estudiante [Carrera] Ingreso                        -- constructor con dos argumentos    
    deriving (Eq, Show)  


-- b)
{-   El constructor (con un argumento) Docente es del tipo (Funcion -> Rol).       -}


-- c)
-- Para programar esta funcion es necesario que los tipos Funcion y Rol deriven en Eq. Y para mostrarlos en pantalla, que pertenezcan a la clase de tipos Show.

cuantos_doc :: [Rol] -> Funcion -> Int
cuantos_doc [] c = 0
cuantos_doc (x:xs) c | x == Docente c = 1 + cuantos_doc xs c
                     | otherwise = cuantos_doc xs c


-- d)

cuantos_doc' :: [Rol] -> Funcion -> Int
cuantos_doc' xs c = length (filter (== Docente c) xs)


{-
   e) Se debe modificar el constructor sin argumento, Decanx, a un constructor que acepte parametros siendo este el genero. Es decir, Decanx Genero.  (*)
-}  
data Genero = Masculino | Femenino 
     deriving (Eq, Show)

{-
   f) El constructor Estudiante, del tipo Rol, acepta dos paramentros, Carrera e Ingreso. Es decir, con la definicion actual no podriamos
      representar a un Estudiante inscripto en dos carreras diferentes, ya que necesitariamos de un nuevo campo, o modificar el existente
	  y reemplazarlo por una lista de Carreras, o sea, Carrera por [Carrera], por ejemplo.  (*)


    Las modificaciones que se realizaron sobre el tipo Rol, pedidas en los ejercicios (*) han sido comentadas e implementadas en el unico tipo Rol que hay,
  no existe Rol' ni Rol'' ya que si crearamos estos tipos con sus modificaciones requeridas, el constructor Estudiante (por ej) pertenecería a tres tipos disntos,
  estos son Rol, Rol' y Rol'' lo cual es imposible y produce error de compilacion.
-}


estudia :: Rol -> Carrera -> Bool
estudia (Estudiante ([]) i ) carrera = False
estudia (Estudiante (x:xs) i) carrera | x == carrera = True
                                      | otherwise = estudia (Estudiante xs i) carrera
estudia _ _ = False



-- Ejercicio 3

-- a)

data Persona = Per String Int Int Rol
    deriving (Eq, Show)

-- b)  Se puede utilizar el mismo identificador para el constructor Per y para el tipo Persona, suele ser comun cuando hay un solo constructor.

-- c)

edad :: Persona -> Int -> Int
edad (Per name dni birth rol) y = y - birth

existe :: String -> [Persona] -> Bool
existe name [] = False
existe name ((Per nomb dni birth rol) : xs) | name == nomb = True
                                            | otherwise = existe name xs

est_astronomia :: [Persona] -> [Persona]
est_astronomia [] = []
est_astronomia ((Per name dni birth (Estudiante [Astronomia] i)) : xs) = (Per name dni birth (Estudiante [Astronomia] i) ) : est_astronomia xs
est_astronomia ((Per name dni birth rol ): xs) = est_astronomia xs


astro (Per name dni birth (Estudiante (c:cs) i)) = (c == Astronomia)
astro _ = False
est_astronomia' :: [Persona] -> [Persona]
est_astronomia' xs = filter (astro) xs


padron_docente :: [Persona] -> [(String, Int)]
padron_docente [] = []
padron_docente ( (Per name dni birth (Docente c)) : xs ) = (name, dni) : padron_docente xs
padron_docente (_:xs) = padron_docente xs


-- Ejercicio 4


data Cola = Vacia | Encolada Persona Cola 
     deriving (Show, Eq)


-- a1)

atender :: Cola -> Cola
atender (Vacia) = Vacia
atender (Encolada _ c) = c

-- a2)

encolar :: Persona -> Cola -> Cola
encolar per Vacia = Encolada per Vacia
encolar per (Encolada per' c) = Encolada per' (encolar per c)

-- a3)

rol :: Persona -> Rol
rol (Per _ _ _ r) = r

busca :: Cola -> Funcion -> Persona
busca Vacia _ = error "No hay nadie que cumpla los requisitos." 
busca (Encolada p c) fn | rol p == Docente fn = p
                        | otherwise = busca c fn

-- b)
{-    El tipo Cola se parece al de listas:
     data Lista a = [] | : a (Lista a)  <=>  data Cola = Vacia | Encolada Persona Cola    -}


-- b1)

atender' :: [Persona] -> [Persona]
atender' [] = []
atender' (x:xs) = xs

-- b2)

encolar' :: Persona -> [Persona] -> [Persona]
encolar' x [] = [x]
encolar' x (y:ys) = y : (encolar' x ys)
--encolar' x (ys) = (ys) ++ [x]

-- b3)

busca' :: [Persona] -> Funcion -> Persona
busca' [] _ = error "No hay nadie que cumpla los requisitos."
busca' (y:ys) fn | (rol y) == Docente fn = y
                 | otherwise = busca' ys fn 



-- Ejercicio 5*


data ListaAsoc a b = LVacia | Nodo a b (ListaAsoc a b)  deriving (Eq, Show)
--a)

type GuiaT = ListaAsoc String Int


--b1)

la_long :: (Integral c) => ListaAsoc a b -> c
la_long LVacia = 0
la_long (Nodo a b ls ) = 1 + la_long ls

-- b2)

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat LVacia a = a
la_concat (Nodo a b p) s = Nodo a b (la_concat p s )

-- b3)
la_pares :: ListaAsoc a b -> [(a,b)]
la_pares LVacia = []
la_pares (Nodo a b ls) = (a,b) : la_pares ls

-- b4)

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca LVacia _ = Nothing
la_busca (Nodo a b ls) k | a == k = Just b
                         | otherwise = la_busca ls k

-- b5)

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar k LVacia = LVacia
la_borrar k (Nodo a b ls) | k == a = ls 
                          | otherwise = la_concat (Nodo a b LVacia) (la_borrar k  ls)


-- Ejercicio 6

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)
     deriving (Eq, Show)

type AGeo = Arbol String


argentina, brasil , italia , america , europa , arbol_del_mundo :: AGeo
argentina = Rama Hoja "Argentina" Hoja
brasil = Rama Hoja " Brasil " Hoja
italia = Rama Hoja " Italia " Hoja
america = Rama argentina "America " brasil
europa = Rama Hoja "Europa " italia
arbol_del_mundo = Rama europa " Tierra " america


-- a)

a_long :: (Integral d) => Arbol a -> d
a_long Hoja = 0
a_long (Rama b _ c) = 1 + (a_long b + a_long c)

-- b)

a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama Hoja _ Hoja) = 2
a_hojas (Rama b a c) = a_hojas b + a_hojas c

-- c)

a_inc :: (Num a) => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama b a c) = Rama (a_inc b) (a+1) (a_inc c)

-- d)

per_a_nom (Per name _ _ _) = name

a_nombre :: Arbol Persona -> Arbol String
a_nombre Hoja = Hoja
a_nombre (Rama b p c) = Rama (a_nombre b) (per_a_nom p) (a_nombre c)

-- e)

a_map :: (a -> b) -> Arbol a -> Arbol b
a_map f Hoja = Hoja
a_map f (Rama b a c) = Rama (a_map f b) (f a) (a_map f c)

a_nombre' :: Arbol Persona -> Arbol b
a_nombre' per_a_nom ar = a_map per_a_nom ar




