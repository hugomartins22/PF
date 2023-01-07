--1)
--a)

intersecs :: Eq a => [a] -> [a] -> [a]
intersecs x [] = []
intersecs [] x = []
intersecs l (c:cs) = (filter (==c) l) ++ (intersecs l cs)


--b)

caudas :: [a] -> [[a]]
caudas [] = [[]]
caudas (l:ls) = (l:ls) : (caudas ls)


--2)

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)


--a)

elems :: ConjInt -> [Int]
elems [] = []
elems ((a,b):ls) = [a..b] ++ (elems ls)


--3)

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]



agenda1 = [("Sofia", [Casa 123456789, Tlm 987654321, Email "abc@def.ghi", Email "f@mendess.xyz"]),("Luís", [Tlm 69420]),("Rita", [Trab 58008])]


--3)
--a)

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n mail age = (n,[(Email mail)]) : age


--b)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((name,c):ls) | (n == name) = Just [aux c]
                          | otherwise = verEmails n ls

 

aux :: [Contacto] -> String
aux [] = []
aux ((Email x):ls) = x
aux (l:ls) = aux ls



--c)

consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta (h:t) = case h of Email x -> (a,b ++ [x])
                           Casa x -> (a ++ [x],b)
                           Trab x -> (a ++ [x],b)
                           Tlm  x -> (a ++ [x],b)
               where (a,b) = consulta t






--d)
{-
consultaIO :: Agenda -> IO ()
consultaIO agenda = do
	nome <-getLine
	let contactos = procuradora nome agenda
	putStr (concat [show x ++ "\n" | x <- contactos])



procuradora :: Nome -> Agenda -> [Contacto]
procuradora name [] = []
procuradora name ((n,c):ls) | (name == n) = c
                            | otherwise = procuradora name ls

-}


--4)

data RTree a = R a [RTree a] 
             deriving (Show, Eq)








encontranodos :: RTree a -> [a]
encontranodos (R a []) = [a]
encontranodos (R a (l:ls)) = a : (encontranodos l)


encontraresto :: RTree a -> [a]
encontraresto (R a []) = [a]
encontraresto (R a (l:ls)) = encontranodos l ++ encontraresto (R a ls) 




tree1 = R 1 [R 2 [],
             R 3 [R 4 [R 5 [],
                       R 6 []
                      ]
                 ],
             R 7 []
            ]