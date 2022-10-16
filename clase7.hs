type Set a = [a]

vacio :: Set Int
vacio = []

agregar :: Int -> Set Int -> Set Int
agregar k (xs) | elem k (xs) = (xs)
               | otherwise = (k:xs)

incluido :: Set Int -> Set Int -> Bool
incluido [] ys = True
incluido (x:xs) ys | elem x ys && incluido xs ys = True
                   | otherwise = False
                       
iguales :: Set Int -> Set Int -> Bool
iguales [] [] = True
iguales xs ys | incluido xs ys && incluido ys xs = True 
              | otherwise = False  

partes :: Int -> Set (Set Int)
partes 0 = [[]]
partes n = partes (n-1) ++ agregarATodos n (partes(n-1))

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs _ = False
parteneceC xs xss | iguales xs (head xss) = True
                  | otherwise = perteneceC xs (tail xss)

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise = (xs : xss)

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos n [] = []
agregarATodos n xss = agregarC (agregar n (head xss)) (agregarATodos n (tail xss))

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] ys = []
productoCartesiano (x:xs) ys = productoAUx x ys ++ productoCartesiano xs ys

productoAUx n [] = []
productoAUx n (x:xs) = [(n,x)] ++ productoAUx n xs