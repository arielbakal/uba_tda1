absoluto :: Int -> Int
absoluto = abs

maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y = maximo ( absoluto x ) ( absoluto y )

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | maximo x y >= z = maximo x y
              | otherwise = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x*y == 0 = True
              | otherwise = False

algunoEs0_p :: Float -> Float -> Bool
algunoEs0_p x 0 = True
algunoEs0_p 0 y = True
algunoEs0_p y x = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | x*y == 0 && x == y = True
              | otherwise = False

ambosSon0_p :: Float -> Float -> Bool
ambosSon0_p 0 0 = True
ambosSon0_p x y = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10 

digitoDecenas :: Int -> Int
digitoDecenas x = div ( mod x 100 ) 10

