
Ejercicio 8.1
Operaciones sobre listas.
-}

-- parte una lista en dos mitades

partir :: [a] -> ([a],[a])
partir [] = ([],[])
partir[x] = ([x],[])
partir (x:y:zs) = (x:xs,y:ys)
	where 
	(xs,ys) = partir zs

-- mezcla dos listas ordenadas en una tercera ordenada

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] ly = ly
mezcla lx [] = lx
mezcla lx@(x:xs) ly@(y:ys)
	| x <= y = x : mezcla xs ly
	| otherwise = y : mezcla lx ys

ordenaMezcla :: Ord a => [a] -> [a]
ordenaMezcla [] = []
ordenaMezcla [x] = [x]
ordenaMezcla zs = mezcla (ordenaMezcla xs) (ordenaMezcla ys)
	where
	(xs,ys) = partir zs

-- inserta un elemento en su posición adecuada en una lista ordenada

insertar :: Ord a => a -> [a] -> [a]
insertar x [] = [x]
insertar x (y:ys)
	|x <= y = x : (y:ys)
	|otherwise = y : insertar x ys

