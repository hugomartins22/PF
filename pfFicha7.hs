data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt


--1)
--a)

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico x) = (- calcula x)
calcula (Mais l r) = (calcula l)+(calcula r)
calcula (Menos l r) = (calcula l)-(calcula r)
calcula (Mult l r) = (calcula l)*(calcula r)




--b)

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico x) = "(-"++ infixa x ++ ")"
infixa (Mais l r) = '(':infixa l++ "+" ++ infixa r ++ ")"
infixa (Menos l r) = '(':infixa l++ "-" ++ infixa r ++ ")"
infixa (Mult l r) = '(':infixa l++ "*" ++ infixa r ++ ")"


--c)

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico x) = (posfixa x) ++ "-"
posfixa (Mais l r) = (posfixa  l)++(posfixa  r)++"+"
posfixa (Menos l r) = (posfixa  l)++(posfixa  r)++"-"
posfixa (Mult l r) = (posfixa  l)++(posfixa  r)++"*"


--2)

data RTree a = R a [RTree a]


soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R x y) = x + sum (map (soma) y)



altura :: RTree a -> Int 
altura (R x []) = 1
altura (R x y) = 1+ (maximum(map (altura) y))


prune :: Int -> RTree a -> RTree a
prune 0 (R x y) = (R x y)
prune n (R x y) = R x (map (prune (n - 1)) y)


mirror :: RTree a -> RTree a
mirror (R x []) = (R x [])
mirror (R x y) = R x (map mirror (reverse y))


postorder :: RTree a -> [a]
postorder (R x [])=[]
postorder (R x y) = (concatMap (postorder) y) ++[x]




--3)

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)


ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork l r) = (ltSum l)+(ltSum r)


listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork l r) = (listaLT l)++(listaLT r)


ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork l r)= maximum ([length(listaLT r)]++[length(listaLT l)])



data FTree a b = Leaf b | No a (FTree a b) (FTree a b)












ltree1 = Fork (Fork (Tip 5)
                    (Fork (Tip 6)
                          (Tip 4)))
              (Fork (Fork (Tip 3)
                          (Tip 7))
                    (Tip 5))
