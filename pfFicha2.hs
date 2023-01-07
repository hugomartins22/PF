
import Data.Char




{-   1)a) y² : cauda
     y²:(head da cauda)² : (cauda da cauda) e assim sucessivamente até alcançar o caso de paragem (que é quando acaba a lista). 
     neste ultimo caso junta ao que ja tinha calculado uma lista vazia retornando a lista calculada ++ [] = lista calculada

[4,9,25,1]



b)[9,12]


c)[]


d) certo

2)
-}

--a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = (x*2) : dobros xs


--b)
numOcorre :: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (l:ls) | (x == l) = 1 + numOcorre x ls
                   | otherwise = numOcorre x ls


--c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (l:ls) | (l>0) = positivos ls
                 | otherwise = False 


--d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (l:ls) | (l>0) = l:(soPos ls)
             | otherwise = soPos ls


--e)
somaNega :: [Int] -> Int
somaNega [] = 0
somaNega (l:ls) | (l<0) = l + (somaNega ls)
                | otherwise = (somaNega ls)



--f)
tresUlt :: [a] -> [a]
tresUlt l | ((length l) < 3) = l
          | otherwise = drop ((length l)-3) l



--g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):ls) = y: (segundos ls)



--h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((x,y):ls) | (a==x) = True
                          | otherwise = nosPrimeiros a ls


{-
--i)
sumTriplos :: (Num a , Num b , Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(x,y,z)] = (x,y,z)
sumTriplos ((x,y,z):ls) = ((x+(sumTriplos ls)),(y+(sumTriplos ls)),(z+(sumTriplos ls)))

-}



--3)

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (l:ls) | (elem l ['0'..'9'] == True) = l : (soDigitos ls)
                 | otherwise = (soDigitos ls)



minusculas :: [Char] -> [Char]
minusculas [] = []
minusculas (l:ls) | (elem l ['a'..'z'] == True) = l : (minusculas ls)
                  | otherwise = (minusculas ls)



nums :: String -> [Int]
nums [] = []
nums (l:ls) | (elem l ['0'..'9'] == True) = ((ord l - ord '0') : (nums ls))
            | otherwise = (nums ls)



type Polinomio =  [Monomio]
type Monomio = (Float,Int)



--4)
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((x,y):ls) | (n==y) = 1 + (conta n ls)
                   | otherwise = (conta n ls)


grau :: Polinomio -> Int
grau [] = 0
grau ((x,y):ls) | y > (grau ls) = y
                | otherwise = grau ls



selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((x,y):ls) | (n == y) = (x,y) : (selgrau n ls) 
                     | otherwise = (selgrau n ls)



deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):ls) = if (y>0) then (((fromIntegral y)*x),(y-1)) : (deriv ls) else (deriv ls)



calcula :: Float -> Polinomio -> Float
calcula n [] = 0
calcula n ((x,y):ls) = (x * n^y) + (calcula n ls)



simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):ls) | (x,y) == (x,0) = simp ls
                | otherwise = (x,y) : simp ls




mult :: Monomio -> Polinomio -> Polinomio
mult (a,b) [] = []
mult (a,b) ((x,y):ls) = ((a*x),(y+b)) : mult (a,b) ls


normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(x,y)]=[(x,y)]
normaliza ((x,y):(x2,y2):ls) | (y==y2) = normaliza ((x2+x,y):ls)
                             | (conta y ls == 0) = (x,y) : normaliza ((x2,y2):ls)
                             | otherwise = normaliza ((x,y):ls) ++ [(x2,y2)]










