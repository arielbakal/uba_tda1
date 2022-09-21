suma n | n == 0 = 0
       | otherwise = suma ( n-1 ) + n

f1 :: Int -> Int
f1 n | n == 0 = 1 
     | otherwise = f1 ( n-1 ) + 2^n

f2 :: Int -> Float -> Float
f2 n q | q^n == q = q
       | otherwise = f2 (n-1) (q) + q^n

f3 :: Int -> Float -> Float
f3 n q | n == 1 = q + q^(2)
       | otherwise = f3 (n-1) (q) + q^(2*n-1) + q^(2*n) 

f4 :: Int -> Float -> Float
f4 n q | n == 0 = 0 
       | otherwise = f4 (n-1) (q) + q^(2*n-1) + q^(2*n) + q^n - ( f2 (n) (q) )

fact n | n == 0 = 1
       | otherwise = n * fact(n-1)

eAprox n | n == 0 = 1
         | otherwise = eAprox (n-1) + ( 1 / fromIntegral( fact n ) )   

doblesuma n m | n == 0 = 0
              | otherwise = f2 m n + doblesuma (n-1) m

              