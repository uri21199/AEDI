-- Ej 1
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1)+fibonacci(n-2)

-- Ej 2
parteEntera :: Float -> Int
parteEntera n | n >= 0 && n < 1 = 0
    | n > 1 = parteEntera (n-1) + 1 

-- Ej 3
esDivisible :: Int -> Int -> Bool
esDivisible x y | x - y == 0 = True
    | x - y < 0 = False
    | otherwise = esDivisible (x - y) y

-- Ej 4
sumaImpares :: Int -> Int
sumaImpares n | n <= 0 = 0
    | otherwise = 2 * (n - 1) + 1 + sumaImpares (n - 1)

-- Ej 5
medioFact :: Int -> Int
medioFact n | n == 0 = 0
    | n == 1 = 1
    | otherwise = n * medioFact(n-2)

-- Ej de clase
cantDigitos :: Int -> Int
cantDigitos n | n <= 10 = 1
    | otherwise = cantDigitos(div n 10) + 1

iesimoDigito :: Int -> Int -> Int
iesimoDigito n m | cantDigitos n == m = mod n 10
                 | otherwise = iesimoDigito (div n 10) m

-- Test
sumaDIg :: Int -> Int
sumaDIg 0 = 0
sumaDIg n = (mod n 10) + sumaDIg (div n 10)

-- Ej 6
todosDigitosIguales :: Int -> Bool
todosDigitosIguales n | 
