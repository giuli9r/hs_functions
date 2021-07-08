data Color = Rojo | Azul | Verde
     deriving (Show, Eq)

data Forma = Triangulo | Cuadrado | Circulo
     deriving (Show, Eq)

type Figura = (Forma, Color, Int)

triang :: Figura -> Bool
triang (f,c,t) = f == Triangulo

cuadradoX :: Figura -> Bool
cuadradoX (f,c,t) = f == Cuadrado

circX :: Figura -> Bool
circX (f,c,t) = f == Circulo

rojoX :: Figura -> Bool
rojoX (f,c,t) = c == Rojo

verdeX :: Figura -> Bool
verdeX (f,c,t) = c == Verde

azulf :: Figura -> Bool
azulf (f,c,t) = c == Azul

--Propiedad F
propF :: [Figura] -> Bool
propF [] = True
propF (x:xs) | ((triang x) && (azulf x)) = False
             | otherwise = propF xs

--Propiedad aux
propII :: [Figura] -> Bool
propII [] = False
propII (x:xs) = (cuadradoX x && rojoX x) || propII xs

--Propiedad I
propI :: [Figura] -> Bool
propI [] = False
propI (x:xs) | (circX x && rojoX x ) = propII xs
             | otherwise = propI xs 