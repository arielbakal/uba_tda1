-- Berton Juan Cruz
-- Bakal Ariel
-- Nabot Teo

-- FUNCIONES AUXILIARES

menorDivAux :: Integer -> Integer -> Integer
menorDivAux n x | mod n x == 0 = x
                | otherwise = menorDivAux n (x+1)

menorDiv :: Integer -> Integer
menorDiv n = menorDivAux n 2

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = menorDiv n == n
           

-- EJERCICIO 1. sonCoprimos

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos x y = sonCoprimosBis x y x

sonCoprimosBis :: Integer -> Integer -> Integer -> Bool
sonCoprimosBis x y z | z == 1 = True
                     | mod x z == 0 && mod y z == 0 = False
                     | otherwise = sonCoprimosBis x y (z-1)


-- EJERCICIO 2. es2Pseudoprimo 

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo 1 = False
es2Pseudoprimo n | esPrimo n == True = False
                 | mod (2^(n-1)-1) n == 0 = True
                 | otherwise = False


-- EJERICIO 3. cantidad3Pseudoprimos

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo 1 = False
es3Pseudoprimo n | esPrimo n == True = False
                 | mod (3^(n-1)-1) n == 0 = True
                 | otherwise = False

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m  =  cantidad3PseudoprimosBis m 1 0

cantidad3PseudoprimosBis :: Integer -> Integer -> Integer -> Integer  
cantidad3PseudoprimosBis m n c | n > m = c
                               | es3Pseudoprimo n == True = cantidad3PseudoprimosBis m (n+1) (c+1)
                               | otherwise = cantidad3PseudoprimosBis m (n+1) c

--EJERICIO 4. kesimo2y3Pseudoprimo

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k = kesimo2y3PseudoprimoAux k 2

kesimo2y3PseudoprimoAux :: Integer -> Integer -> Integer
kesimo2y3PseudoprimoAux k n | k == 0 = n-1
                            | es2Pseudoprimo n && es3Pseudoprimo n = kesimo2y3PseudoprimoAux (k-1) (n+1)
                            | otherwise = kesimo2y3PseudoprimoAux k (n+1)

--EJERCICIO 5. esCarmichael

esCarmichael :: Integer -> Bool
esCarmichael n | esPrimo n == True = False
               | otherwise = esCarmichaelAux n 2


esCarmichaelAux :: Integer -> Integer -> Bool
esCarmichaelAux n a | n == a = True
                    | sonCoprimos n a == False = esCarmichaelAux n (a+1)
                    | esAPseudoprimoOptimizada n a == True = esCarmichaelAux n (a+1)  
                    | otherwise = False

esAPseudoprimoOptimizada :: Integer -> Integer -> Bool
esAPseudoprimoOptimizada 1 a = False
esAPseudoprimoOptimizada n a = mod (a^(n-1)-1) n == 0
--esAPseudoprimoOptimizada  es optimizada porque no decide si n es primo o no, ya que ya lo hace la función inicial (esCarmichael)