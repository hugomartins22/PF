
import Data.List
import Data.Char

data Frac = F Integer Integer

--1)

normaliza :: Frac -> Frac
normaliza (F c b) | (c>0 && b>0) = F (div c divisor) (div b divisor)
                  | (c>0 && b<0) = F (div c divisor) (-(div (abs b) divisor))
                  | (c<0 && b>0) = F (div (abs c) divisor) (-(div b divisor))
                  | otherwise = F (abs(div c divisor)) (abs(div b divisor))
                where divisor = mdc (abs c) (abs b)


mdc :: Integer ->Integer->Integer 
mdc a b | (a == b) = a 
        | (a > b) = mdc (a - b) b 
        | otherwise = mdc b a 




--b)

instance Eq Frac where
  (F a b) == (F c d) = a * d == c * b



--c)

instance Ord Frac where
  (F a b) >= (F c d) =  a * d >= c * b


--d)

instance Show Frac where
  show (F a b) = (show a) ++ "/" ++ (show b)



--e)

instance Num Frac where
    (F a b) + (F c d) | b == d = normaliza $ F (a + c) b
                      | otherwise = normaliza $ F (a * d + b * c) (b * d)
    x - y = x + negate y
    (F a b) * (F c d) = F (a * c) (b * d)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) | a == 0 = 0
                   | a * b > 0 = 1
                   | otherwise = (-1)
    fromInteger x = F x 1





--2)

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)



--a)

instance Show a => Show (Exp a) where
  show (Const a) = (show a)
  show (Simetrico a) = "(-" ++ (show a) ++ ")"
  show (Mais a b) = "(" ++ (show a) ++ "+" ++ (show b) ++ ")"
  show (Menos a b) = "(" ++ (show a) ++ "-" ++ (show b) ++ ")"
  show (Mult a b) = "(" ++ (show a) ++ "*" ++ (show b) ++ ")"

