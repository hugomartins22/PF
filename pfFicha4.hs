import Data.Char

--1)a)
--[5,12,18]

--b)
--[5,12,18]

--c)
--[(11,19),(10,20),(12,18),(13,17),)(14,16)(15,15),(14,16),(13,17),(12,18),(11,19),(10,20)]



--2)
--a)
--[y^x| x<-[0..10]]

--b)
--[(x,y)| x<-[1..5],y<-[1..5],(x+y)==6]

--c)
--[[1..x]| x<-[1..5]]

--d)
--[replicate x 1 , x<-[1..5] ]



--3)
-- isdigit, isAlpha ja existem , char -> Bool


digitAlpha :: String -> (String,String)
digitAlpha = foldl (\(alpha,digit) x -> if isAlpha x then (alpha++[x],digit) else if isDigit x then (alpha,digit++[x]) else (alpha,digit)) ("","")



--4)
nzp :: [Int] -> (Int,Int,Int)
nzp = foldl (\(neg,zero,pos) x -> if auxpos x then (neg,zero,pos+1) else if auxneg x then (neg +1,zero,pos) else if auxzero x then (neg,zero+1,pos) else (neg,zero,pos)) (0,0,0) 



auxpos :: Int -> Bool
auxpos n = if n>0 then True else False



auxneg :: Int -> Bool
auxneg n = if n<0 then True else False



auxzero :: Int -> Bool
auxzero n = if n==0 then True else False





--6)
optimize :: [Int] -> Int
optimize = foldl (\ lista x -> x+10*lista) 0




