f1 :: (Ord a, Floating a) => a -> a -> a -> Bool
f1 x y z = x**y + z <= x+y**z

f2 :: (Floating a) => a -> a -> a
f2 x y = sqrt x / sqrt y

f3 :: (Integral a, Floating a) => a -> a -> a
f3 x y = div (sqrt x) (sqrt y) 

f4 :: (Eq a, Floating a) => a -> a -> a -> a
f4 x y z | x == y = z
         | x ** y == y = x
         | otherwise = y

f5 :: (Eq a, Floating a) => a -> a -> b -> b
f5 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z
         
estanRelacionados :: (Ord a, Ord b, Num a, Num b) => a -> b -> Bool
estanRelacionados x y | x <= 3 && y <= 3 = True
                      | x >  7 && y >  7 = True  
                      | (x > 3 && x <= 7) && (y > 3 && y <= 7) = True
                      | otherwise = False 

prodInt :: (Num a) => (a, a) -> (a, a) -> a
prodInt (x1,x2) (y1,y2) = x1*y1 + x2*y2

todoMenor :: (Ord a) => (a, a) -> (a, a) -> Bool
todoMenor (x1,x2) (y1,y2) | x1 < y1 && x2 < y2 = True
                          | otherwise = False

distanciaPuntos :: (Floating a) => (a, a) -> (a, a) -> a
distanciaPuntos (x1,x2) (y1,y2) = sqrt( ( y1 - x1 )**2 + ( y2 - x2 )**2 )

sumaTerna :: (Integral a) => (a, a, a) -> a
sumaTerna (x1,x2,x3) = x1 + x2 + x3

esPar :: (Integral a) => a -> Bool
esPar x | mod x 2 == 0 = True
        | otherwise = False

posicPrimerPar :: (Integral a, Integral b, Integral c, Num t) => (a, b, c) -> t
posicPrimerPar (x1,x2,x3) | esPar x1 = 1
                          | esPar x2 = 2
                          | esPar x3 = 3
                          | otherwise = 4 

crearPar :: a -> b -> (a, b)
crearPar x y = (x,y)

invertir :: a -> b -> (b, a)
invertir x y = (y,x) 

