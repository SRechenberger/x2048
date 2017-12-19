module Main where

import Control.Comonad

import Control.Monad.Random (MonadRandom, uniform, evalRand, getRandomR)
import System.Random (StdGen, getStdGen)
import System.IO (stdout, hSetBuffering, BufferMode (..))
import System.Environment (getArgs)

import Data.List.Lens

import Data.List (intercalate, maximumBy, minimumBy)

import Data.Maybe (catMaybes, isNothing)

import Data.Function (on)


-------------------------------------------------------------------------------
-- Utils ----------------------------------------------------------------------
-------------------------------------------------------------------------------

type Point = (Int, Int)

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,a,_) = a

thr3 :: (a,b,c) -> c
thr3 (_,_,a) = a

log2 :: (Ord a, Num a) => a -> a
log2 num = log2' 1 0
  where
    log2' n x
        | n >= num  = x
        | otherwise = log2' (n*2) (x+1)

avg :: [Int] -> Int
avg as = sum as `div` length as

on2 :: (left -> right -> result) -> (input -> left) -> (input -> right) -> input -> result
on2 op left right input = left input `op` right input

coeff :: [Double]
coeff = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1]

coeff' :: [Double]
coeff' = drop 5 coeff

coeff2 :: [(Double,Double)]
coeff2 = (,) <$> coeff' <*> coeff'

-------------------------------------------------------------------------------
-- Game -----------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Basics ---------------------------------------------------------------------

data Game a = Game Point a a a a a a a a a a a a a a a a
    deriving (Eq, Show, Ord)

newgame :: a -> Game a
newgame a = Game (0,0) a a a a  a a a a  a a a a  a a a a

setFocus :: Point -> Game a -> Game a
setFocus p (Game _  a b c d  e f g h  i j k l  m n o q) = Game p  a b c d  e f g h  i j k l  m n o q

getFocus :: Game a -> Point
getFocus (Game p  _ _ _ _  _ _ _ _  _ _ _ _  _ _ _ _) = p

getField :: Point -> Game a -> a
getField p = setFocus p >>> extract

setField :: Point -> a -> Game a -> Game a
setField p v = extend (\g -> if getFocus g == p then v else extract g)

instance Functor Game where
    fmap f (Game p  a00 a10 a20 a30  a01 a11 a21 a31  a02 a12 a22 a32  a03 a13 a23 a33) = Game p
        (f a00) (f a10) (f a20) (f a30)
        (f a01) (f a11) (f a21) (f a31)
        (f a02) (f a12) (f a22) (f a32)
        (f a03) (f a13) (f a23) (f a33)

instance Comonad Game where
    extract (Game (0,0) a _ _ _  _ _ _ _  _ _ _ _  _ _ _ _) = a
    extract (Game (1,0) _ a _ _  _ _ _ _  _ _ _ _  _ _ _ _) = a
    extract (Game (2,0) _ _ a _  _ _ _ _  _ _ _ _  _ _ _ _) = a
    extract (Game (3,0) _ _ _ a  _ _ _ _  _ _ _ _  _ _ _ _) = a

    extract (Game (0,1) _ _ _ _  a _ _ _  _ _ _ _  _ _ _ _) = a
    extract (Game (1,1) _ _ _ _  _ a _ _  _ _ _ _  _ _ _ _) = a
    extract (Game (2,1) _ _ _ _  _ _ a _  _ _ _ _  _ _ _ _) = a
    extract (Game (3,1) _ _ _ _  _ _ _ a  _ _ _ _  _ _ _ _) = a

    extract (Game (0,2) _ _ _ _  _ _ _ _  a _ _ _  _ _ _ _) = a
    extract (Game (1,2) _ _ _ _  _ _ _ _  _ a _ _  _ _ _ _) = a
    extract (Game (2,2) _ _ _ _  _ _ _ _  _ _ a _  _ _ _ _) = a
    extract (Game (3,2) _ _ _ _  _ _ _ _  _ _ _ a  _ _ _ _) = a

    extract (Game (0,3) _ _ _ _  _ _ _ _  _ _ _ _  a _ _ _) = a
    extract (Game (1,3) _ _ _ _  _ _ _ _  _ _ _ _  _ a _ _) = a
    extract (Game (2,3) _ _ _ _  _ _ _ _  _ _ _ _  _ _ a _) = a
    extract (Game (3,3) _ _ _ _  _ _ _ _  _ _ _ _  _ _ _ a) = a

    extract (Game p _ _ _ _  _ _ _ _  _ _ _ _  _ _ _ _) = error $ "Game.extract: focus " ++ show p ++ " not valid."

    duplicate g@(Game p  _ _ _ _  _ _ _ _  _ _ _ _  _ _ _ _) = Game p
        (setFocus (0,0) g) (setFocus (1,0) g) (setFocus (2,0) g) (setFocus (3,0) g)
        (setFocus (0,1) g) (setFocus (1,1) g) (setFocus (2,1) g) (setFocus (3,1) g)
        (setFocus (0,2) g) (setFocus (1,2) g) (setFocus (2,2) g) (setFocus (3,2) g)
        (setFocus (0,3) g) (setFocus (1,3) g) (setFocus (2,3) g) (setFocus (3,3) g)


-- Control --------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Game Tree ------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Main -----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = putStrLn "Fuck you!"
