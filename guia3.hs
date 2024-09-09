import Prelude hiding ((-))
-- Ej 1
f :: Int -> Int
f 1 = 8
f 4 = 131
f 16 = 16

g :: Int -> Int
g 8 = 16
g 16 = 4
g 131 = 1

-- Ej 2a
-- problema absoluto (a:Z) : Z {
-- requiere: {True}
-- asegura: {res = valor absoluto de a}
-- }
absoluto :: Int -> Int
absoluto x = x
absoluto -x = x

-- Ej 2b
-- problema mamximoabsoluto (a:Z, b:Z) : Z {
-- requiere: {uso recursivo de la funcion absoluto}
-- asegura: {si a > b -> res = a}
-- asegura: {si b > a -> res = b}
-- }
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y = max (absoluto x) (absoluto y)

-- Ej 3
-- problema maximo3 (a:Z, b:Z, c:Z) : Z{
-- requiere: {True} 
-- asegura: {si a > b y a >= c -> res = a}
-- asegura: {si b > a y b>= c -> res = b}
-- asegura: {si c > a y c>= b -> res = b}
-- }
maximo3 :: Int-> Int -> Int -> Int
maximo3 a b c | a >= b && a >= c = a
    | b >= a && b >= c = b
    | otherwise = c

-- Ej 2d
-- problema algunoEs0 (a:R, b:R) : Bool {
-- requiere: {True}
-- asegura: {si a = 0 -> res = True}
-- asegura: {si b = 0 -> res = True}
-- asegura: {si a y b diferentes a cero -> res = False}
-- }
algunoEs0 :: Float -> Float -> Bool
algunoEs0 a b = a == 0 || b == 0

algunoEs0_ 0 _ = True
algunoEs0_ _ 0 = True
algunoEs0_ _ _ = False

-- Ej 2e
-- problema ambosSon0 (a:R, b:R) : Bool {
-- requiere: {True}
-- asegura: {si a == 0 y b == 0 -> res = True}
-- }
ambosSon0 :: Float -> Float -> Bool
ambosSon0 a b = a == 0 && b == 0

ambosSon0_ 0 0 = True
ambosSon0_ _ _ = False

-- Ej 2f
-- problema mismoIntervalo (a:R, b:R) : Bool{
-- requiere: {True}
-- asegura: {a y b pertenecen al mismo intervalo}
-- }
mismoIntervalo :: Int -> Int -> Bool
mismoIntervalo a b | a <= 3 && b <= 3 = True
    | a > 3 && a <= 7 && b > 3 && b <= 7 = True
    | a >= 7 && b >= 7 = True
    | otherwise = False

-- Ej 2g
-- problema sumaDistintos (a:Z, b:Z, c:Z) : Z{
-- requiere: {True}
-- asegura: {si a = b y a != c -> res = a + c}
-- asegura: {si b = c -> res = a + c}
-- asegura: {si c = a -> res = b + c}
-- asegura: {a, b c son distintos entonces res = a + b + c}
-- }

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos a b c
    | a /= b && a /= c && b /= c = a + b + c
    | a == b && a /= c           = a + c
    | b == c && a /= b           = a + b
    | c == a && c /= b           = a + b    
    | otherwise                  = 0


-- Ej 2h
-- problema esMultiploDe (a:N, b:N) : Bool{
-- requiere: {True}
-- asegura: {si a = k * b siendo k una constante -> res = True}
-- }

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b | mod a b == 0 = True
    | otherwise = False

-- Ej 2i
-- problema digitoUnidades (a:Z) : Int{
-- requiere: {True}
-- asegura: {cantidad de digitos de a}
-- } 

digitoUnidades :: Int -> Int
digitoUnidades a = mod (abs a) 10

-- Ej 2j
-- problema digitoDecenas (a:Z) : Int{
-- requiere: {a > 9}
-- asegura: {extraer dígitos de a}
-- }
digitoDecenas :: Int -> Int
digitoDecenas x = digitoUnidades (div (absoluto x) 10) 

-- Ej 3
estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b | mod a b == 0 = True
    | otherwise = False 

-- Ej 4a
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (a1, a2) (b1, b2) = a1 * a2 + b1 * b2

-- Ej 4b
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a1, a2) (b1, b2) | a1 < b1 && a2 < b2 = True
    | otherwise = False

-- Ej 4c
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a1, a2) (b1, b2) = sqrt (a1 - b1)^2 + (a2 - b2)^2

-- Ej 4d
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a,b,c) = a + b + c

-- Ej 4e
sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (a, b, c) d 
    | mod a d == 0 && mod b d == 0 && mod c d /= 0 = a + b  
    | mod a d /= 0 && mod b d == 0 && mod c d == 0 = b + c
    | mod a d == 0 && mod b d /= 0 && mod c d == 0 = a + c
    | mod a d == 0 && mod b d == 0 && mod c d == 0 = a + b + c
    | mod a d == 0 = a
    | mod b d == 0 = b 
    | mod c d == 0 = c
    | otherwise = 0    

-- Ej 4f
posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (x, y, z) | mod x 2 == 0 = 0
                       | mod y 2 == 0 = 1
                       | mod z 2 == 0 = 2
                       | otherwise = 4

-- Ej 4g
crearPar :: a -> b -> (a, b) 
crearPar a b = ((a), (b))

-- Ej 4h
invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

-- Ej 5
todosMenores :: (Integer, Integer, Integer) ->Bool
todosMenores (a,b,c) = f(a) > g(a) && f(b) > g(b) && f(c) > g(c)

fTM :: Int -> Int
fTM n | n <= 7 = n*n
    | n > 7 = 2*n-1

gTM :: Int -> Int
gTM n | mod n 2 == 0 = div n 2
    | otherwise = 3*n + 1

-- Ej 6
bisiesto :: Int -> Bool
bisiesto a | mod a 4 == 0 && mod a 100 == 0 && mod a 400 /= 0 = True
    | otherwise False

-- Ej 7
distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a,b,c) (d,e,f) = abs(a - d) + abs(b - e) + abs(c - f)

-- Ej 8
comparar :: Int -> Int -> Int
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
    | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
    | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0             

sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x = (mod (abs x) 10) + (mod (div (abs x) 10) 10)