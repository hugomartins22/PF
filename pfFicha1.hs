import Data.Char

--exercicio1 a

perimetro :: Double -> Double
perimetro r = 2*pi*r


--exercicio1 b

distance :: (Double,Double)-> (Double,Double)-> Double
distance (x1,y1) (x2,y2) = sqrt ( ((x2-x1)^2) + ((y2-y1)^2))


--exercicio1 c

fstLast :: [Int] -> (Int,Int)
fstLast (x:xs) = (x,(last xs))


--exercicio1 d

multiplo :: Int -> Int -> Bool
multiplo n m | (mod n m) == 0 = True
             | otherwise = False


--exercicio1 e

truncaImpar :: [Int] -> [Int]
truncaImpar l | (odd (length l) == True) = (tail l)
              | otherwise = l


--exercicio1 f

max2 :: Int -> Int -> Int
max2 a b | a<b = b
         | otherwise = a


--exercicio1 g

max3 :: Int -> Int -> Int -> Int
max3 a b c | c > (max2 a b) = c
           | otherwise = (max2 a b)


--exercicio2 a
-- calcula o nº de raizes de um polinomio

formResolvente :: Int -> Int -> Int -> Int
formResolvente a b c | ((b^2-4*a*c) == 0) && (a/=0) && (b/=0) = 1
                     | ((b^2-4*a*c) < 0) = 0
                     | otherwise = 2 



--exercicio2 b
-- calcula as raizes reais do polinomio

--raizesReais :: Int -> Int -> Int -> [Int]
--raizesReais a b c | ((formResolvente a b c)==0) = []
--                  | ((formResolvente a b c)==1) = [((-b) / (2*a))]
--                  | otherwise = [(((-b) + (sqrt (b^2 - 4*a*c))) / (2*a)), (((-b) - (sqrt (b^2 - 4*a*c))) / (2*a))]  




-- exercicio3 a

horavalida :: (Int,Int) -> Bool
horavalida (h,m) | (h>=0 && h<=23) && (m>=0 && m<=59) = True
                 | otherwise = False 




