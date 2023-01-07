--1)

--a)
any1 :: (a->Bool)->[a]->Bool
any1 f [] = False
any1 f (l:ls) | (f l == True) = True
             | otherwise = any f ls  



--b)
zipWith1 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith1 f [] [] = []
zipWith1 f [x] [] = []
zipWith1 f [] [x] = []
zipWith1 f (x:xs) (y:ys) = (f x y) : zipWith f xs ys




--c)
takes :: (a->Bool) -> [a] -> [a]
takes f [] = []
takes f (l:ls) | (f l == True) = l : takes f ls
               | otherwise = [] 



--d)
drops :: (a->Bool) -> [a] -> [a]
drops f [] = []
drops f (l:ls) | (f l == True) = drops f ls
               | otherwise = (l:ls) 




--e)
spans :: (a->Bool) -> [a] -> ([a],[a])
spans f [] = ([],[])
spans f (l:ls) | f l = (l:s1,s2)
               | otherwise = ([],(l:ls))  
               where (s1,s2) = spans f ls



--f)
deleteByes :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteByes f x [] = []
deleteByes f x (l:ls) | f x l = ls
                      | otherwise =l: (deleteByes f x ls)



--2)

type Polinomio = [Monomio]
type Monomio = (Float,Int)


--a)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n l = filter (\ x -> (snd x==n)) l


--b)

conta :: Int -> Polinomio -> Int
conta n l = length (filter (\ x -> (snd x == n)) l)


--c)

grau :: Polinomio -> Int
grau l = foldl (\maiorgrau x -> if (snd x)<maiorgrau then maiorgrau else (snd x)) 0 l

--d)

deriv :: Polinomio -> Polinomio
deriv l = map (\ (cof,grau) -> ((fromIntegral grau)*cof,grau-1)) l


--e)

calcula :: Float -> Polinomio -> Float
calcula n = foldl (\valor (cof,grau) -> cof*n^grau + valor) 0


--f)

simp :: Polinomio -> Polinomio
simp l = filter (\ x -> fst x>0) l


--g)

mult :: Monomio -> Polinomio -> Polinomio
mult (cof,grau) l = map (\ (x,y) -> (x*cof,y+grau)) l 


--3)

--a)

type Mat a = [[a]]


dimOK :: Mat a -> Bool
dimOK [] = True
dimOK [x] = True
dimOK (l1:l2:ls) | (length l1) == (length l2) = dimOK (l2:ls)
                 | otherwise = False



