sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1) 

esPrimo p | p == 1 = False
          | (menorDivisorDesde p 2) == p = True  
          | otherwise = False  

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1) 

nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n - 1))

minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n + 1) 

