
import System.Random
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Control.Monad


--1)

bingo :: IO ()
bingo = do 
    n <- acumulador []
           print n



acumulador [Int] -> IO [Int]
acumulador l | (length l) == 90 = do return l
             | otherwise = do v <- randomRIO (1,90)
                                   print v
                                   getChar
                                   let nl = if v `elem` l then l else v:l in acumulador nl