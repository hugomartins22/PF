
--1)
--a)

insert :: Ord a => a -> [a] -> [a]
insert n [] = []
insert n (l:ls) | (n<=l) = n : (l:ls)
                | otherwise = l : (insert n ls)

--2)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just a):ls) = a : (catMaybes ls)
catMaybes ((Nothing):ls) = (catMaybes ls)



--3)

data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)




instance (Show a) => Show (Exp a) where
  show (Const a) = show a
  show (Var a) = a
  show (Mais x y) = "("++(show x)++ " + " ++ (show y) ++ ")"
  show (Mult x y) = "("++(show x)++ " x " ++ (show y) ++ ")"




--5)
--a)

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = (maximum l) - (minimum l)

--6)

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]


ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                          Quadrado 4,
                          Mover (4,3) (Quadrado 2)])


--a)

conta :: Imagem -> Int
conta (Quadrado x) = 1
conta (Mover (x,y) i) = conta i
conta (Juntar l) = sum (map (conta) l)



--b)

apaga :: Imagem -> IO Imagem
apaga i = do
      let imagens = remover i
      putStrLn (show imagens)


--randomRIO (0..(conta i))



remover :: Imagem -> Imagem
remover (Quadrado x) = Juntar []
remover (Mover (x,y) i) = remover i
remover (Juntar l) = sum(map (remover) l)
