--Ejercicio 1

--[1,0..(-100)]

--[(-19),(-15)..20]

--Ejercicio 2

sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = head l + sumatoria (tail l)

sumatoria_p :: [Int] -> Int
sumatoria_p [] = 0
sumatoria_p (x:xs) = sumatoria_p xs + x

longitud :: [Int] -> Int
longitud l = longitudAux l 0

longitudAux :: [Int] -> Int -> Int
longitudAux l y | l == [] = y
                | otherwise = longitudAux (tail l) (y+1)

longitud_ :: [Int] -> Int
longitud_ l | l == [] = 0
            | otherwise = 1 + longitud_ (tail l)

longitud_p :: [Int] -> Int
longitud_p [] = 0
longitud_p (x:xs) = 1 + longitud xs

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | x == head l = True
              | otherwise = pertenece x (tail l) 

pertenece_ :: Int -> [Int] -> Bool
pertenece_ x l | l == [] = False
               | otherwise = (x == head l) || (pertenece x (tail l))

pertenece_p :: Int -> [Int] -> Bool
pertenece_p e [] = False
pertenece_p e (x:xs) = (e == x) || (pertenece_p e xs)

--Ejercicio 3

productoria :: [Int] -> Int
productoria l | l == [] = 1
              | otherwise = head l * productoria (tail l)

productoria_p :: [Int] -> Int
productoria_p [] = 1
productoria_p (x:xs) = x * productoria_p xs

sumarN :: Int -> [Int] -> [Int]
sumarN n l | l == [] = []
           | otherwise = (head l + n) : (sumarN n (tail l))  

sumarN_p :: Int -> [Int] -> [Int]
sumarN_p n [] = []
sumarN_p n (x:xs) = (x+n) : (sumarN_p n xs)

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero l | l == [] = []
                 | otherwise = sumarN (head l) l

sumarElPrimero_p :: [Int] -> [Int]
sumarElPrimero_p [] = []
sumarElPrimero_p (x:xs) = sumarN x (x:xs)


elUltimo :: [Int] -> Int
elUltimo [] = undefined
elUltimo [x] = x 
elUltimo (x:xs) = elUltimo xs

sumarElUltimo :: [Int] -> [Int] 
sumarElUltimo l | l == [] = []
                | otherwise = sumarN (elUltimo l) l

sumarElUltimo_p :: [Int] -> [Int] 
sumarElUltimo_p [] = []
sumarElUltimo_p (x:xs) = sumarN (elUltimo (x:xs)) (x:xs)

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | mod x 2 == 0 = (x : pares xs)
             | otherwise = pares xs

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | mod x n == 0 = (x : multiplosDeN n xs) 
                      | otherwise = multiplosDeN n xs

reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

maximonum :: Int -> Int -> Int
maximonum n m | n > m = n
              | n < m = m 
              | otherwise = m

maximo :: [Int] -> Int
maximo [] = 0
maximo (x:xs) = maximonum x (maximo xs) 

ordenard :: [Int] -> [Int]
ordenard [] = []
ordenard l = maximo l : ordenard (quitar (maximo l) l)

ordenar :: [Int] -> [Int]
ordenar l = reverso (ordenard l)

quitar :: Int -> [Int] -> [Int]
quitar n [] = []                    
quitar n (x:xs) | n == x = quitar n (xs)
                | otherwise = (x : quitar n (xs)) 

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

--eliminarRepetidos puede ser el mismo que "ordenar" pues el enunciado es ambiguo

