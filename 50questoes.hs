--1)

enumFromTo2 :: Int -> Int -> [Int]
enumFromTo2 a b | (a==b) = [b]
                | otherwise = a : (enumFromTo2 (a+1) b)
              

--2)

enumFromThenTo2 :: Int -> Int -> Int -> [Int]
enumFromThenTo2 a b c | (a==c) = [c]
                      | (a>c) = []
                      | otherwise = a : (enumFromThenTo2 (a+b) b c)



--3)

conca :: [a] -> [a] -> [a]
conca [] [] = []
conca [] (c:cs) = c : (conca [] cs)
conca (l:ls) (c:cs) = l : (conca ls (c:cs))



--4)

encontra :: [a] -> Int -> a
encontra (l:ls) n | (n==0) = l
                  | otherwise = (encontra ls (n-1))


--5)

oupainverte :: [a] -> [a]
oupainverte [l] = [l]
oupainverte (l:ls) = (oupainverte ls) ++ [l]



--6)

takamos :: Int -> [a] -> [a]
takamos n [] = []
takamos 0 (l:ls) = []
takamos n (l:ls) = l : (takamos (n-1) ls)



--7)

dropamos :: Int -> [a] -> [a]
dropamos n [] = []
dropamos 0 x = x
dropamos n (l:ls) = dropamos (n-1) ls



--8)

zipamos :: [a] -> [b] -> [(a,b)]
zipamos [] x = []
zipamos x [] = []
zipamos (l:ls) (c:cs) = (l,c) : (zipamos ls cs)



--9)

elevamos :: Eq a => a -> [a] -> Bool
elevamos n [] = False
elevamos n (l:ls) | (n==l) = True
                  | otherwise = elevamos n ls


--10)

replicas :: Int -> a -> [a]
replicas 0 x = []
replicas n x = x : (replicas (n-1) x)



--11)

intermovistar :: a -> [a] -> [a]
intermovistar x [y] = [y]
intermovistar x (l:ls) = [l] ++ [x] ++ (intermovistar x ls)


--12)

agrupamos :: Eq a => [a] -> [[a]]
agrupamos [] = []
agrupamos (l:ls) = [l:(takeWhile (==l) ls)] ++ (agrupamos (dropWhile (==l) ls))


--13)

concamos :: [[a]] -> [a]
concamos [] = []
concamos (l:ls) = l ++ (concamos ls)


--14)

anita :: [a] -> [[a]]
anita [] = [[]]
anita (l:ls) = anita (init (l:ls)) ++ [(l:ls)]


--15)

rabiote :: [a] -> [[a]]
rabiote [] = [[]]
rabiote (l:ls) = [(l:ls)] ++ (rabiote ls)


--16)

preTreino :: Eq a => [a] -> [a] -> Bool
preTreino [] x = True
preTreino x [] = False
preTreino (l:ls) (c:cs) | (l == c) = (preTreino ls cs)
                        | otherwise = False



--17)

posTreino ::  Eq a => [a] -> [a] -> Bool
posTreino [] c = True
posTreino c [] = False
posTreino l c | (last l)==(last c) = posTreino (init l) (init c)
              | otherwise = False


--18)

consequencia :: Eq a => [a] -> [a] -> Bool
consequencia [] x = True
consequencia x [] = False
consequencia (l:ls) (c:cs) | (l==c) = (consequencia ls (c:cs))
                           | otherwise = (consequencia (l:ls) cs)



--19)

piceta :: Eq a => a -> [a] -> [Int]
piceta n [] = []
piceta n (l:ls) | (n==l) = 0 : (map (+1) (piceta n ls))
                | otherwise = (map (+1) (piceta n ls))


--20)

bernardo :: Eq a => [a] -> [a]
bernardo [x] = [x]
bernardo (l1:l2:ls) | (l1==l2) = bernardo (l2:ls)
                    | (elem l1 (l2:ls)) == False = l1 : (bernardo (l2:ls))
                    | otherwise = bernardo (l2:ls)


--21)

apaga :: Eq a => a -> [a] -> [a]
apaga n [] = []
apaga n (l:ls) | (n==l) = ls
               | otherwise = l : (apaga n ls)


--22)

barrasfds :: Eq a => [a] -> [a] -> [a]
barrasfds x [] = x
barrasfds (l:ls) (c:cs) | (l==c) = barrasfds ls cs
                        | otherwise = l : (filter (/=c) (barrasfds ls cs))


--23)

cebolas :: Eq a => [a] -> [a] -> [a]
cebolas [] x = x
cebolas x [] = x
cebolas (l:ls) (c:cs) | (elem c (l:ls))==True = (cebolas (l:ls) cs)
                      | otherwise = (cebolas (l:ls) cs) ++ [c]


--24)

insetos :: Eq a => [a] -> [a] -> [a]
insetos [] x = []
insetos x [] = []
insetos (l:ls) (c:cs) | (elem l (c:cs))==True = l: (insetos ls (c:cs))
                      | otherwise = insetos ls (c:cs)


--25)

ninoladentro :: Ord a => a -> [a] -> [a]
ninoladentro n [] = [n]
ninoladentro n (l:ls) | (n<l) = n : (l:ls)
                      | otherwise = l: (ninoladentro n ls)


--26)

palavras :: [String] -> String
palavras [x] = x
palavras (l:ls) = l ++ " " ++ (palavras ls)


--27)

linhas :: [String] -> String
linhas [] = ""
linhas (l:ls) = l ++ "\n" ++ (linhas ls)



--28)

fodilhona :: Ord a => [a] -> Int
fodilhona l = procuralista (maior l) l


maior :: Ord a => [a] -> a
maior [x] = x
maior (l:ls) = maior (filter (>l) ls)


procuralista :: Eq a => a -> [a] -> Int
procuralista n [] = 0
procuralista n (l:ls) | (n==l) = 0
                      | otherwise = 1 + (procuralista n ls)


--29)

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (l:ls) | (filter (==l) ls) == [] = (temRepetidos ls)
                    | otherwise = True


--30)

algarismos :: [Char] -> [Char]
algarismos [] = [] 
algarismos (l:ls) | (elem l ['0'..'9'])==True = l : (algarismos ls)
                  | otherwise = algarismos ls



--31)

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (l1:l2:ls) = l2 : (posImpares ls)



--32)

pares :: [a] -> [a]
pares [] = []
pares [x] = [x]
pares (l1:l2:ls) = l1 : (pares ls)


--33)

sortido :: Ord a => [a] -> Bool
sortido [] = True
sortido (l:ls) | (filter (<l) ls)== [] = (sortido ls) 
               | otherwise = False


--34)

ohSorte :: Ord a => [a] -> [a]
ohSorte [] = []
ohSorte (l:ls) = ninoladentro l (ohSorte ls)


--35)

hija :: String -> String -> Bool
hija [] x = True
hija x [] = False
hija (l:ls) (c:cs) | (l<=c) = (hija ls cs)
                   | otherwise = False



--36)

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet n [] = False
elemMSet n ((a,i):ls) | (n==a) = True
                      | otherwise = elemMSet n ls 


--37)

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,i):ls) = i + (lengthMSet ls)



--38)

converting:: [(a,Int)] -> [a]
converting [] = []
converting ((a,i):ls) = (replicate i a) ++ converting ls


--39)

estoucansado :: Eq a => a -> [(a,Int)] -> [(a,Int)]
estoucansado n [] = [(n,1)] 
estoucansado n ((a,i):ls) | (n==a) = ((a,i+1):ls)
                          | otherwise =(a,i) : estoucansado n ls



--40)

removes :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removes n [] = []
removes n ((a,i):ls) | (n==a) = ls
                     | otherwise = (a,i) : (removes n ls)


--41)

bob :: Ord a => [a] -> [(a,Int)]
bob [] = []
bob (l:ls) = reverse (estoucansado l (bob ls))



--42)

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left x):ls) = x:partitionLefts ls
          partitionLefts ((Right x):ls) = partitionLefts ls
          partitionRights [] = []
          partitionRights ((Left x):ls) = partitionRights ls
          partitionRights ((Right x):ls) = x:partitionRights ls


--43)

gatinho :: [Maybe a] -> [a]
gatinho [] = []
gatinho ((Just a):ls) = a : (gatinho ls)
gatinho ((Nothing):ls) = (gatinho ls)



--44)

data Movimento = Norte | Sul | Este | Oeste
               deriving Show


posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y) 
posicao (x,y) ((Norte):ls) = posicao (x,y+1) ls
posicao (x,y) ((Sul):ls) = posicao (x,y-1) ls
posicao (x,y) ((Este):ls) = posicao (x+1,y) ls
posicao (x,y) ((Oeste):ls) = posicao (x-1,y) ls




--45)

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | (xf-xi)<0 && (yf-yi)<0 = (replicate (aux3 xf xi) Oeste)++(replicate (aux3 yf yi) Sul)
                        | (xf-xi)>0 && (yf-yi)<0= (replicate (aux3 xf xi) Este)++(replicate (aux3 yf yi) Sul)
                        | (xf-xi)>0 && (yf-yi)>0= (replicate (aux3 xf xi) Este)++(replicate (aux3 yf yi) Norte)
                        | otherwise = (replicate (aux3 xf xi) Oeste)++(replicate (aux3 yf yi) Norte)




aux3 :: Int -> Int -> Int
aux3 a b = abs(a-b)





--46)

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (Este:ls) = False
vertical (Oeste:ls) = False
vertical (Norte:ls) = vertical ls
vertical (Sul:ls) = vertical ls


vertical2 :: [Movimento] -> Bool
vertical2 [] = True
vertical2 (l:ls)= case l of Este -> False
                            Oeste -> False
                            Sul -> vertical2 ls
                            otherwise -> vertical2 ls





--47)

data Posicao = Pos Int Int
             deriving Show


maisCentral :: [Posicao] -> Posicao
maisCentral = foldl1 (\(Pos a b) (Pos c d) -> if (a+b) > (c+d) then (Pos c d) else (Pos a b)) 



--48)

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos a [] = []                   
vizinhos a (b:ls) | (diferencas a b)==True = b : (vizinhos a ls)
                  | otherwise = (vizinhos a ls)




diferencas :: Posicao -> Posicao -> Bool
diferencas (Pos x y) (Pos a b) | (abs(x-a)==1) || (abs(y-b)==1) = True
                               | otherwise = False 




--49)

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x y):(Pos a b):ls) | (y==b) = (mesmaOrdenada ((Pos a b):ls))
                                       | otherwise = False 





--50)

data Semaforo = Verde | Amarelo | Vermelho
              deriving Show




interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l | ((auxilio l) <= 1) = True
                | otherwise = False 





auxilio :: [Semaforo] -> Integer
auxilio [] = 0
auxilio (Verde:ls) = 1 + auxilio ls
auxilio (Amarelo:ls) = 0 + auxilio ls
auxilio (Vermelho:ls) = 0 + auxilio ls


