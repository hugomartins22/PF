
--1)



arvore1 = (Node 5 (Node 2 (Node 1 Empty
                                  Empty) 
                          (Node 3 Empty 
                                  Empty)) 
                  (Node 9 (Node 7 (Node 6 Empty 
                                          Empty) 
                                  (Node 8 Empty 
                                          Empty)) 
                          Empty))

data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show




--a)

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)



--b)


contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node n e d) = 1 + (contaNodos e) + (contaNodos d)


--c)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node n Empty Empty) = 1
folhas (Node x e s) = (folhas e) + (folhas s)


--d)

prune :: Int -> BTree a -> BTree a
prune 0 x = Empty
prune n (Node p x y) = (Node p (prune (n-1) x) (prune (n-1) y))  

--e)

path :: [Bool] -> BTree a -> [a]
path [] x = []
path (t:ts) (Node fst l r) | (t==False) = fst : (path ts l)
                           | otherwise = fst : (path ts r)


--f)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node f l r) = (Node f (mirror r) (mirror l))


--g)


zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT func Empty b = Empty
zipWithBT func a Empty = Empty
zipWithBT func (Node f1 l1 r1) (Node f2 l2 r2) = (Node (func f1 f2) (zipWithBT func l1 l2) (zipWithBT func r1 r2))


--h)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) l r) = (Node a unzipL1 unzipR1,Node b unzipL2 unzipR2,Node c unzipL3 unzipR3)
                           where (unzipL1,unzipL2,unzipL3) = unzipBT l
                                 (unzipR1,unzipR2,unzipR3) = unzipBT r


--2)
--a)

minimo :: Ord a => BTree a -> a
minimo (Node f Empty r) = f 
minimo (Node f l r) = minimo l



--b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty _) = Empty
semMinimo (Node f l r) = (Node f (semMinimo l) r)



--c)

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node e Empty _) = (e,Empty)
minSmin (Node e l r) = (a,Node e b r)
        where (a,b) = minSmin l


--d)

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r) | x < e = Node e (remove x l) r
                      | x > e = Node e l (remove x r)
                      | otherwise = aux x (Node e l r)
    where aux n (Node a b c) = case b of Empty -> c
                                         otherwise -> case c of Empty -> b
                                                                otherwise -> Node g b h
          (g,h) = minSmin r






--3)

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
                   deriving Show
type Turma = BTree Aluno




turma1 :: Turma
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty) 
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                        (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))


--a)

inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node f l r) | ((aux1 f)== n) = True
                       | otherwise = inscNum n (if n < (aux1 f) then l else r)



aux1 :: Aluno -> Numero
aux1 (n,name,r,c) = n






--b)

inscNome :: Nome -> Turma -> Bool
inscNome name Empty = False
inscNome name (Node f l r) | (name==(aux2 f)) = True
                           | otherwise = inscNome name l


inscNome2 :: Nome -> Turma -> Bool
inscNome2 name Empty = False
inscNome2 name (Node f l r) | (inscNome name (Node f l r) == True) = True
                            | otherwise = inscNome2 name r


aux2 :: Aluno -> Nome
aux2 (n,name,r,c) = name


--c)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (b,nom,TE,c) l r) = (trabEst l) ++ [(b,nom)] ++ (trabEst r)
trabEst (Node (b,nom,t,c) l r) = (trabEst l) ++ (trabEst r)



--d)

nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota n (Node (num,name,st,c) l r) | (inscNum n (Node (num,name,st,c) l r))== False = Nothing
                                  | (n==num) = Just c
                                  | otherwise = (nota n (if n<num then l else r))

