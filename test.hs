-- Aprende Haskell por el bien de todos.
-- Creacion de tipos

data Person = Person { name :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phNumber :: String
                     , flavor :: String
                     } deriving (Show, Eq)

data Car = Car { company :: String, model :: String, year :: Int } deriving (Show)

{-
 Una clase de tipos es una especie de interfaz que define un comportamiento.
 Cuando decimos que un tipo es una instancia de un clase de tipos,
 estamos diciendo que podemos usar las funciones de esa clase de tipos con ese tipo.
 
  Es muy importante distinguir entre constructores de datos y constructores de tipo. 
  Cuando declaramos un tipo de dato, la parte anterior al = es el constructor de tipos,
  mientras que la parte que va después (posiblemente separado por |) son los constructores de datos.
-}

-- Creacion de un tipo parametrico Vector (ijk) y operaciones asociadas.

data Vector t = Vector t t t   deriving (Show, Eq)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector x y z) = Vector (i+x) (j+y) (k+z)

vmult :: (Num t) => Vector t -> Vector t -> Vector t
vmult (Vector i j k) (Vector x y z) = Vector (i*x) (j*y) (k*z)

vscalar :: (Num t) => Vector t -> t -> Vector t
vscalar (Vector i j k) n = Vector (i*n) (j*n) (k*n)

-- Tipo de dato perteneciente a todas las clases de tipos

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
     deriving (Show, Eq, Enum, Ord, Bounded, Read)


data List a = Empty | Cons {listHead :: a, listTail :: List a }
     deriving (Show, Eq, Read, Ord)

data Lista a = Vacia | Const a (Lista a)
     deriving (Show, Eq, Read, Ord)

{-  Arbol binario de busqueda  -}

data Tree a = ArVacio | Node a (Tree a) (Tree a)
    deriving (Eq, Show, Ord)

-- funcion que cree un arbol unitario (con dos sub-arboles vacios), a partir de un elemento

single :: a -> Tree a
single x = Node x (ArVacio) (ArVacio)

-- funcion que inserta un elemento en un arbol

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x ArVacio = single x
insertTree x (Node a left right) | x == a = Node x left right
								 | x < a = Node a (insertTree x left) right
								 | x > a = Node a left (insertTree x right)

-- funcion que compruebe si un elemento pertenece a un arbol

treeElement :: (Ord a) => a -> Tree a -> Bool
treeElement x ArVacio = False
treeElement x (Node a left right) | x == a = True
                                  | x < a = treeElement x left
								  | x > a = treeElement x right




















