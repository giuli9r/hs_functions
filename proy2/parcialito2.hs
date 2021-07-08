{- Giuliano Rossetti   DNI 38.108.666	TEMA  -}



{- Ejercicio 1. Medalla -}


data Medalla = Bronce | Plata | Oro   deriving (Eq, Show, Ord)

type Medallero = [Medalla]

valor_medalla :: Medalla -> Int
valor_medalla Bronce = 1
valor_medalla Plata = 2
valor_medalla Oro = 4



{- Ejercicio 2 -}

data Disciplina = Boxeo | Judo | Vela | Jockey | Tenis

data Resultado = Res Disciplina Medalla

medallero_vela :: [Resultado] -> Medallero
medallero_vela [] = []
medallero_vela ((Res Vela medalla) : rs) = medalla : medallero_vela rs
medallero_vela (_ : rs) = medallero_vela rs



{- Ejercicio 3 -}

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)   deriving (Show)

la_existe :: (Eq a) => ListaAsoc a b -> a -> Bool
la_existe Vacia k = False
la_existe (Nodo a b la) k | k == a = True
                          | otherwise = la_existe la k




la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia clave = Nothing
la_busca (Nodo a b la) clave | a == clave = Just b
                             | otherwise = la_busca la clave

{-
la_existe' :: (Eq a) => ListaAsoc a b -> a -> Bool
la_existe' (la) k | la_busca (la) k == (Just _) = True
                  | otherwise = False
-}


{- Ejercicio 4 -}

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a) deriving Show

arbol_busca :: Arbol (Int, String) -> Int - > Maybe String
arbol_busca Hoja k = Nothing
arbol_busca (Rama (ar_1) (k,s) (ar_2)) k = Just s











