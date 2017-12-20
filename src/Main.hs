{-# LANGUAGE LambdaCase #-}
module Main (main) where

import qualified Control.Category as Cat
import Control.Arrow
import Control.Comonad
import Control.Monad (join, forM)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar)

import Control.Monad.Random (MonadRandom, uniform)
-- import System.Random (StdGen, getStdGen)
-- import System.IO (stdout, hSetBuffering, BufferMode (..))
import System.Environment (getArgs)


import Data.List (find, intercalate, maximumBy, minimumBy)
import Data.Foldable (Foldable (..))
-- import Data.Bifunctor (Bifunctor (..))

import Data.Maybe (isNothing, isJust, maybe)

import Data.Function (on)


-------------------------------------------------------------------------------
-- Utils ----------------------------------------------------------------------
-------------------------------------------------------------------------------

type Point = (Int, Int)

type Matrix a = Vector (Vector a)
type Vector a = (a,a,a,a)

split :: Arrow arrow => arrow a (a,a)
split = Cat.id &&& Cat.id


ntimes :: Int -> (a -> a) -> a -> a
ntimes n f 
    | n < 1     = id
    | otherwise = f . ntimes (n-1) f


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

replaceFirst :: Eq a => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst x y (x':xs) 
    | x == x'   = y:xs
    | otherwise = x':replaceFirst x y xs

fix :: (a -> a) -> a
fix f = f (fix f)

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldl (+) 0 

maximum' :: (Foldable t, Ord a) => t a -> a
maximum' = foldl1 max 

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


zipWithGame :: (a -> b -> c) -> Game a -> Game b -> Game c
zipWithGame f ga = extend (\gb -> f (getField (getFocus gb) ga) (extract gb))


zipGame :: Game a -> Game b -> Game (a,b)
zipGame = zipWithGame (,)


rotate :: Game a -> Game a
rotate = extend (\g -> getFocus >>> rotatePoint >>> flip getField g $ g)
  where
    rotatePoint :: Point -> Point
    rotatePoint (x,y) = (3-y,x)


rotateN :: Int -> Game a -> Game a
rotateN n = ntimes n rotate


getGameLine :: Int -> Game a -> [a]
getGameLine n g = [ getField (x,n) g | x <- [0..3] ]


setGameLine :: Int -> [a] -> Game a -> Game a
setGameLine n l
    | n >= 4 || n < 0 = error $ "setline: invalid line number " ++ show n
    | length l /= 4   = error $ "setline: invalid line lenght " ++ show (length l)
setGameLine n l =
    extend (\g -> let (_,n') = getFocus g in if n' == n then l else getGameLine n' g)
    >>> extend (\g -> let (x,_) = getFocus g in extract g `idx` x)
  where
    idx [a,_,_,_] 0 = a
    idx [_,a,_,_] 1 = a
    idx [_,_,a,_] 2 = a
    idx [_,_,_,a] 3 = a
    idx l' i = error $ "idx: invalid arguments (list lenght " ++ show (length l') ++ ", index " ++ show i ++ ")"


toMatrix :: Game a -> (Point, Matrix a)
toMatrix (Game focus a b c d  e f g h  i j k l  m n o p) = (focus,((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)))

fromMatrix :: Point -> Matrix a -> Game a
fromMatrix focus ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) = (Game focus  a b c d  e f g h  i j k l  m n o p) 


instance Foldable Game where
    foldr f r (Game _ a b c d  e q g h  i j k l  m n o p) =
        a `f` (b `f` (c `f` (d `f` (e `f` (q `f` (g `f` (h `f` (i `f` (j `f` (k `f` (l `f` (m `f` (n `f` (o `f` (p `f` r)))))))))))))))


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

    extract (Game p _ _ _ _  _ _ _ _  _ _ _ _  _ _ _ _) = error $ "Game.extract: invalid focus " ++ show p

    duplicate g@(Game p  _ _ _ _  _ _ _ _  _ _ _ _  _ _ _ _) = Game p
        (setFocus (0,0) g) (setFocus (1,0) g) (setFocus (2,0) g) (setFocus (3,0) g)
        (setFocus (0,1) g) (setFocus (1,1) g) (setFocus (2,1) g) (setFocus (3,1) g)
        (setFocus (0,2) g) (setFocus (1,2) g) (setFocus (2,2) g) (setFocus (3,2) g)
        (setFocus (0,3) g) (setFocus (1,3) g) (setFocus (2,3) g) (setFocus (3,3) g)


-- Game Control ---------------------------------------------------------------

type Game2048 = Game (Maybe Int)

ppGame :: Game2048 -> String
ppGame = toList
    >>> map (\case Nothing -> "    "; Just x -> show x ++ "")
    >>> fix (\f xs -> if null xs then [] else take 4 xs : f (drop 4 xs))
    >>> map (intercalate "\t")
    >>> intercalate "\n"

data Dir = L | D | R | U
    deriving (Show, Eq)


rotateToDir :: Dir -> Game a -> Game a
rotateToDir L = rotateN 0
rotateToDir U = rotateN 1
rotateToDir R = rotateN 2
rotateToDir D = rotateN 3


rotateFromDir :: Dir -> Game a -> Game a
rotateFromDir L = rotateN 0
rotateFromDir U = rotateN ((-1) `mod` 4)
rotateFromDir R = rotateN ((-2) `mod` 4)
rotateFromDir D = rotateN ((-3) `mod` 4)


freeField :: Game2048 -> Bool
freeField = extract >>> isNothing


freeFields :: Game2048 -> Game Bool
freeFields = extend freeField


compressLine' :: [Maybe Int] -> ([Maybe Int],[Int])
compressLine' [] = ([],[])
compressLine' [x] = ([x],[])
compressLine' l@(Nothing:xs) = case join (find isJust xs) of
    Nothing -> (l,[])
    x       -> compressLine' (x:replaceFirst x Nothing xs)
compressLine' (x@(Just _):xs) = case join (find (== x) xs) of
    Nothing -> let (xs',cs) = compressLine' xs in (x:xs',cs)
    Just x' -> let (xs',cs) = compressLine' (replaceFirst x Nothing xs) in (Just (2*x'):xs',x':cs)


compressLines :: Game2048 -> (Game2048, [Int])
compressLines g = foldr
    (\i (gacc,cs) -> let (l,cs') = (getGameLine i >>> compressLine') gacc in (setGameLine i l gacc,cs++cs'))
    (g,[])
    [0,1,2,3]


playerMove :: Dir -> Game2048 -> (Game2048, [Int])
playerMove dir = rotateToDir dir >>> compressLines >>> first (rotateFromDir dir)


possiblePlayerMoves :: Game2048 -> [Dir]
possiblePlayerMoves g = filter (\d -> fst (playerMove d g) /= g) [L,U,R,D]

possibleCPUMoves :: Game2048 -> [Point]
possibleCPUMoves g = filter (\p -> isNothing (getField p g)) [(x,y) | x <- [0..3], y <- [0..3]]

writeRandomField :: MonadRandom m => Game2048 -> m Game2048
writeRandomField g = do
    p <- uniform $ possibleCPUMoves g
    pure $ setField p (Just 2) g

-------------------------------------------------------------------------------
-- Game Simulation ------------------------------------------------------------
-------------------------------------------------------------------------------

data Player = PLAYER | CPU
    deriving (Show, Eq)

-- http://iamkush.me/an-artificial-intelligence-for-the-2048-game/
expectimax :: (Game2048 -> Double) -> Int -> Game2048 -> Double
expectimax = expectimax' CPU

expectimax' :: Player -> (Game2048 -> Double) -> Int -> Game2048 -> Double
expectimax' _ _ n _ | n < 0 = error "Fuck..."
expectimax' _ score 0 g = score g
expectimax' CPU score n g = case [prob * expectimax' PLAYER score (n-1) (setField p (Just 2) g) | p <- moves] of
    [] -> 0.0
    ms -> sum' ms
  where
    moves = possibleCPUMoves g
    prob  = 1 / toEnum (length moves)
expectimax' PLAYER score n g = case [expectimax' CPU score (n-1) (fst $ playerMove d g) | d <- moves] of
    [] -> 0.0
    ms -> maximum' ms
  where
    moves = possiblePlayerMoves g


logScore :: Game2048 -> Double
logScore = fmap (maybe 0.0 toEnum)
  >>> fmap (logBase 2.0)
  >>> split                           
  >>> first maximum'                 
  >>> uncurry (flip (/) >>> fmap)
  --- >>> args snd fst (flip (/) >>> fmap)
  >>> sum'                           

simpleScore :: Game2048 -> Double
simpleScore = fmap (maybe 0 id)
  >>> sum'
  >>> toEnum

betterScore :: Game2048 -> Double
betterScore = extend (\g -> simpleScore g  * neighbour g * posScore g) >>> sum'

posScore :: Game a -> Double
posScore = getFocus >>> uncurry (+) >>> toEnum >>> (/ 6)

neighbour :: Game2048 -> Double
neighbour g = sum' >>> (/ 8)$ n
  where
    (x,y) = getFocus g
    n = [ 1.0
        | dx <- [-1,0,1], dy <- [-1,0,1]
        , 0 <= dx + x, dx + x <= 3
        , 0 <= dy + y, dy + y <= 3
        , dx /= 0 || dy /= 0 
        , getField (x+dx,y+dy) g == extract g] 

pp :: String -> Game2048 -> IO ()
pp msg g = do
    putStrLn msg
    ppGame >>> putStrLn $ g

play :: Player -> Int -> Int -> Game2048 -> IO (Game2048,Int)
play CPU depth s g = case possibleCPUMoves g of
    [] -> pure (g,s)
    _  -> writeRandomField g >>= play PLAYER depth s
play PLAYER depth s g = case possiblePlayerMoves g of
    [] -> pure (g,s)
    ds -> do
        -- putStr $ show s ++ "\r"
        let (_, (g',pts)) = maximumBy (compare `on` (snd >>> fst >>> expectimax betterScore depth)) [(d,playerMove d g) | d <- ds]
        play CPU depth (s + sum pts) g'


-------------------------------------------------------------------------------
-- Main -----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    [depth,tests] <- getArgs
    results' <- forM [1..read tests] $ \i -> do
        mvar <- newEmptyMVar
        forkIO $ do
            (final,pts) <- play CPU (read depth) 0 (newgame Nothing)
            putStrLn $ "Thread " ++ show i ++ " finished."
            putMVar mvar (final,pts)
        putStrLn $ "Thread " ++ show i ++ " sparked."
        pure mvar
    results <- mapM readMVar results'
    let (wG,wP) = minimumBy (compare `on` snd) results
    let (bG,bP) = maximumBy (compare `on` snd) results
    let average = unzip >>> snd >>> avg $ results

    putStrLn "----------------------------------------------------------------"
    pp ("WORST: " ++ show wP ++ " pts") wG
    putStrLn ""
    pp ("BEST:  " ++ show bP ++ " pts") bG
    putStrLn ""
    putStrLn $ "AVERAGE: " ++ show average ++ " pts"
