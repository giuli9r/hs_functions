-- asd de funciones currificadas


-- comparador con 100
compare1 :: (Num a, Ord a) => a -> Ordering
compare1 x = compare 100 x

--comparador con n
compare2 :: (Num a, Ord a) => a -> a -> Ordering
compare2 x n = compare n x


-- funcion que hace negativo un numero.
negn :: (Num a) => a -> a
negn n = n * (-1)

-- funcion qu niega dos veces
neg2 :: (t -> t) -> t -> t
neg2 f x = f (f x)

-- funcion que aplica f dos veces
aplica2 :: (a -> a) -> a -> a
aplica2 f x = f (f x)
