-- PUNTO NÚMERO 1
-- Ej 1
longitud :: [t] -> Int
longitud _[] = 0
longitud [x] = 1
longitud (x:xs) = 1 + longitud xs

-- Ej 2
ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- Ej 3
principio :: [t] -> [t]
principio [x:[]] = x
principio (x:xs) = x : principio xs

-- Ej 4
reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = reverso (xs) ++ [x]

-- PUNTO NÚMERO 2
-- Ej 1
pertenece :: (Eq t) -> t -> [t] -> Bool
pertenece _ [] = False
pertenece x(y:ys) = x == y || pertenece x ys

-- Ej 2
todosIguales :: (Eq t) -> [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) | longitud xs == 1 = x == head xs
    | x/=head xs = False
    | otherwise todosIguales xs

-- Ej 3
todosDistintos :: (Eq t) -> [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) | pertenece x xs = False
    | otherwise = todosDistintos xs

-- Ej 4
hayRepetidos :: (Eq t) -> [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

-- Ej 5
quitar :: (Eq t) -> t -> [t] -> [t]
quitar _ [] = []
quitar elem (x:xs) | elem == x = xs
    | otherwise = x : quitar elem xs

-- Ej 6
quitarTodos :: (Eq t) -> t -> [t] -> [t]
quitarTodos : [] = 
