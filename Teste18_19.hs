
import Data.Char

--1)


elemIndices2 :: Eq a => a -> [a] -> [Int]
elemIndices2 n [] = []
elemIndices2 n (l:ls) | (n==l) = 0 : map (+1) (elemIndices2 n ls)
                      | otherwise = map (+1) (elemIndices2 n ls)




isSub :: Eq a => [a] -> [a] -> Bool
isSub [] x = True
isSub (l:ls) (c:cs) | (l==c) = isSub ls cs
                    | (elem l (c:cs)) = isSub (l:ls) cs
                    | otherwise = False





--2)

data BTree a = Empty | Node a (BTree a) (BTree a)
             deriving Show



lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP n Empty = Nothing
lookupAP n (Node (a,b) l r) | (n==a) = (Just b)
                            | otherwise = lookupAP n (if n<a then l else r)




zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f x Empty = Empty
zipWithBT f (Node a l r) (Node b l2 r2) = (Node (f a b) (zipWithBT f l l2) (zipWithBT f r r2))



--3)

digitAlpha :: String -> (String,String)
digitAlpha = foldl (\(a,b) x -> if (isDigit x)==True then (a++[x],b) else (a,b++[x])) ("","")




data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)


--4)
--a)

firstSeq :: Seq a -> a
firstSeq (Cons a b) = a
firstSeq (App Nil b) = firstSeq b
firstSeq (App x b) = firstSeq x


pila = (App (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) (Cons 1 Nil))

--b)

dropSeq :: Int -> Seq a -> Seq a
dropSeq n Nil = Nil
dropSeq n (Cons a x) = (dropSeq (n-1) x)
dropSeq n (App a b) | (n<(contanodos a)) = (dropSeq (n-1) a)
                    | otherwise = (dropSeq (n-(contanodos a)) b)  


contanodos :: Seq a -> Int
contanodos Nil = 0
contanodos (Cons a x) = 1 + (contanodos x)
contanodos (App a b) = (contanodos a) + (contanodos b)



--c)

instance Show a => Show (Seq a) where
  show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons a Nil) = show a
mostra (Cons a s) = show a ++ "," ++ mostra s
mostra (App s1 s2) = mostra s1 ++ "," ++ mostra s2




--5)
--a) é para os mongos

--b)

magic :: Mat Int -> Bool














arvore2 = (Node 5 (Node 2 (Node 1 Empty
                                  Empty) 
                          (Node 3 Empty 
                                  Empty)) 
                  (Node 9 (Node 7 (Node 6 Empty 
                                          Empty) 
                                  (Node 8 Empty 
                                          Empty)) 
                          Empty))





arvore1 = (Node (5,4) (Node (7,4) (Node (2,3) Empty
                                  Empty) 
                          (Node (2,4) Empty 
                                  Empty)) 
                  (Node (6,4) (Node (1,4) (Node (2,6) Empty 
                                          Empty) 
                                  (Node (2,8) Empty 
                                          Empty)) 
                          Empty))
