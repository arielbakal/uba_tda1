fib :: Int -> Int
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)  

parteEntera :: Float -> Int
parteEntera x | x < 1 = 0
              | x >= 1  = 1 + parteEntera( x - 1 )  

multiplo3 :: Int -> Bool
multiplo3 n | n == 3 = True
            | n < 3 = False
            | n > 3 = multiplo3 ( n - 3 )

nesimoImpar :: Int -> Int
nesimoImpar n = 2*n - 1

sumaImpares :: Int -> Int 
sumaImpares n | n == 0 = 0
              | otherwise = (nesimoImpar n) + (sumaImpares( n - 1 ))

medioFact :: Int -> Int
medioFact n | n <= 2 = n
            | otherwise = n * medioFact( n - 2 )

digitoUnidad :: Int -> Int
digitoUnidad x = mod x 10

digitoDecena :: Int -> Int
digitoDecena x = digitoUnidad( div x 10 )

sumaDigitos :: Int -> Int
sumaDigitos x | x < 10 = x
              | x >= 10 = digitoUnidad x + sumaDigitos(div x 10)

digitosIguales :: Int -> Bool 
digitosIguales x | x < 10 = True
                 | x >= 10 = digitoUnidad x == digitoDecena x && digitosIguales (div x 10)
                 
