--Ejercicio 1

--[1,0..(-100)]

--[(-19),(-15)..20]


--Ejercicio 2

sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = head l + sumatoria (tail l)

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

longitud_p [] = 0
longitud_p (x:xs) = 1 + longitud xs

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | x == head l = True
              | otherwise = pertenece x (tail l) 

pertenece_ :: Int -> [Int] -> Bool
pertenece_ x l | l == [] = False
               | otherwise = (x == head l) || (pertenece x (tail l))

pertenece_p e [] = False
pertenece_p e (x:xs) = (e == x) || (pertenece_p e xs)

--Ejercicio 3

productoria :: [Int] -> Int
productoria l | l == [] = 1
              | otherwise = head l * productoria (tail l)

productoria_p :: [Int] -> Int
productoria_p [] = 1
productoria_p (x:xs) = x * productoria_p xs

sumarN n l | l == [] = []
           | otherwise = (head l + n) : (sumarN n (tail l))  

sumarN_p n [] = []
sumarN_p n (x:xs) = (x+n) : (sumarN_p n xs)

sumarElPrimero l | l == [] = []
                 | otherwise = sumarN (head l) l

sumarElPrimero_p [] = []
sumarElPrimero_p (x:xs) = sumarN x (x:xs)

elUltimo (x:[]) = x 
elUltimo (x:xs) = elUltimo xs

sumarElUltimo l | l == [] = []
                | otherwise = sumarN (elUltimo l) l

sumarElUltimo_p [] = []
sumarElUltimo_p (x:xs) = sumarN (elUltimo (x:xs)) (x:xs)

reverso [] = []
reverso (x:xs) = reverso xs ++ [x]