suma n | n == 0 = 0
       | otherwise = suma ( n-1 ) + n

f1 :: Integer -> Int
f1 n | n == 0 = 1 
     | otherwise = f1 ( n-1 ) + 2^n

f2 :: Integer -> Float -> Float
f2 n q | q^n == q = q
       | otherwise = f2 (n-1) (q) + q^n

f3 :: Integer -> Float -> Float
f3 n q | n == 1 = q + q^(2)
       | otherwise = f3 (n-1) (q) + q^(2*n-1) + q^(2*n) 

f4 :: Integer -> Float -> Float
f4 n q | n == 0 = 0 
       | otherwise = f4 (n-1) (q) + q^(2*n-1) + q^(2*n) + q^n - ( f2 n q )

fact :: Integer -> Integer
fact n | n == 0 = 1
       | otherwise = n * fact(n-1)

eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | otherwise = eAprox (n-1) + ( 1 / fromIntegral( fact n ) )   

doblesuma :: Integer -> Integer -> Integer
doblesuma n m | n == 0 = 0
              | otherwise = round (f2 m (fromIntegral n)) + doblesuma (n-1) m

sumaPotencias :: Float -> Integer -> Integer -> Float
sumaPotencias q n 0 = 0
sumaPotencias q n m = (sumaPotencias q n (m-1)) + q^m*( f2 n q )

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n 0 = 0
sumaRacionales 0 m = 0 
sumaRacionales n m = sumaRacionales n (m-1) + fromIntegral (suma n) / fromIntegral m

--Tarea

g1 :: Integer -> Integer -> Integer
g1 0 n = 0
g1 i n | n == 1 = i
       | otherwise = g1 i (n-1) + i^n

g2 :: Integer -> Integer
g2 n | n == 1 = 1
     | otherwise = g2 (n-1) + n^n           

g3 :: Integer -> Integer
g3 n | n == 1 = 4
     | otherwise = g3 (n-1) + 2^(2*n)

contardigitos :: Integer -> Integer
contardigitos d | d < 10 = 1
                | otherwise = contardigitos( div d 10 ) + 1     

sumadigitos :: Integer -> Integer
sumadigitos n = suma ( 10^(contardigitos n) -1 )