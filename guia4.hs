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
sumaImpares | 1 = 1
            | sumaImpares (n-1)

-- Ej 5
medioFact :: Int -> Int
medioFact | n >= 0 = 


-- Ej de clase
cantDigitos :: Int -> Int
cantDigitos | n <= 10 = 1
            | otherwise = cantDigitos(div n 10) + 1

iesimoDigito :: Int -> Int
iesimoDigito |