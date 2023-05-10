-- Bakal Ariel bakalariel2002@gmail.com
-- Berton Juan Cruz juanceberton@gmail.com 
-- Nabot Teo teonabot@gmail.com


type Complejo = (Float, Float)

moduloReal :: Float -> Float
moduloReal x | x >= 0 = x
             | x < 0 = -x

resta :: Complejo -> Complejo -> Complejo
resta (a,b) (c,d) = (a-c,b-d)
             
--1.1
re :: Complejo -> Float
re (a,b) = a

--1.2
im :: Complejo -> Float
im (a,b) = b

--1.3
suma :: Complejo -> Complejo -> Complejo
suma (a,b) (c,d) = (a+c,b+d)

--1.4
producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = (a*c - b*d , a*d + b*c)

--1.5
conjugado :: Complejo -> Complejo
conjugado (a,b) = (a,-b)

--1.6
inverso :: Complejo -> Complejo                        -- inverso = conjugado / modulo^2
inverso (a,b) = ( (a / modz^2 ) , (-b / modz^2 ) )
                where modz = modulo (a,b) 

--1.7
cociente :: Complejo -> Complejo -> Complejo           -- z/w = z * w^-1
cociente (a,b) (c,d) = producto (a,b) (inverso (c,d))

--1.8
potencia :: Complejo -> Integer -> Complejo
potencia (a,b) k | k == 1 = (a,b)
                 | otherwise = producto (a,b) (potencia (a,b) (k-1))

--1.9
raicesCuadraticas :: Float -> Float -> Float -> (Complejo, Complejo)
raicesCuadraticas a b c | d >= 0 = ((r1,0), (r2,0))
                        | d < 0 = (w, conjugado w)                 
                        where d = b^2 - 4 * a * c                  --discriminante
                              w = ((-b)/ (2*a), (sqrt (-d))/(2*a)) --raiz compleja 
                              r1 = (-b + sqrt(d)) / (2*a)          --raices reales 
                              r2 = (-b - sqrt(d)) / (2*a)           

--2.1
modulo :: Complejo -> Float
modulo (a,b) = sqrt( (a)^2 + (b)^2 )

--2.2
distancia :: Complejo -> Complejo -> Float
distancia z w = modulo (re z - re w , im z - im w )

--2.3
argumento :: Complejo -> Float                                
argumento (a,b) | a == 0 && b > 0 = pi/2
                | a == 0 && b < 0 = (3*pi)/2  
                | a > 0 && b >= 0 = atan(b/a)                         --1° cuadrante
                | a < 0 && b >= 0 = pi - moduloReal (atan(b/(-a)))    --2° 
                | a < 0 && b < 0  = pi + moduloReal (atan(-b/(-a)))   --3° 
                | a > 0 && b < 0  = (2*pi) - moduloReal (atan(-b/a))  --4°
                   
--2.4
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r t = (r*cos(t),r*sin(t))

--2.5
raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada z = ( pasarACartesianas (sqrt(modulo z)) ((argumento z)/2) , pasarACartesianas (sqrt(modulo z)) ((argumento z)/2 + pi) )

--2.6
raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo, Complejo)
raicesCuadraticaCompleja a b c = (cociente (suma (-re b, -im b) w) (producto (2,0) a), cociente (resta (-re b, -im b) w) (producto (2,0) a)) 
                                where d = resta (potencia b 2) (producto (producto (4,0) a)  c)
                                      w = snd(raizCuadrada d)
--3.1
raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = raicesNEsimasAux (fromIntegral n) (fromIntegral (n-1))


raicesNEsimasAux :: Float -> Float -> [Complejo]
raicesNEsimasAux n k | k == 0 = [(1,0)]
                     | otherwise = [pasarACartesianas 1 ((2*k*pi)/n)] ++ raicesNEsimasAux n (k-1)

--3.2
sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesNEsimas n [] e = True
sonRaicesNEsimas n z e | ((modulo (resta (potencia (head z) n) (1,0)) < e) == False) = False 
                       | otherwise = sonRaicesNEsimas n (tail z) e 
                  
