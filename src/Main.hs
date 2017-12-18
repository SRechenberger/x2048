{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Comonad
import Control.Monad (forM)

import Control.Monad.Random (MonadRandom, uniform, evalRand, getRandomR)
import System.Random (StdGen, getStdGen)

import Data.List.Lens

import Data.List (intercalate, maximumBy, minimumBy)

import Data.Maybe (catMaybes, isNothing)

import Data.Function (on)


-------------------------------------------------------------------------------
-- Utils ----------------------------------------------------------------------
-------------------------------------------------------------------------------

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

type Point = (Int, Int)

fst3 (a,_,_) = a
snd3 (_,a,_) = a
thr3 (_,_,a) = a

-------------------------------------------------------------------------------
-- Game -----------------------------------------------------------------------
-------------------------------------------------------------------------------

newtype Game2048 a = Game { _field :: [[Maybe a]] }
    deriving (Show, Eq)

makeLenses ''Game2048

    
instance Functor Game2048 where
    fmap f (Game ass) = Game $ fmap (fmap (fmap f)) ass

newgame :: Game2048 a
newgame = Game $ replicate 4 (replicate 4 Nothing)

ppGame2048 :: Show a => Game2048 a -> String
ppGame2048 (Game fs) = intercalate "\n" $ map (intercalate "\t" . map show') $ fs
  where
    show' Nothing = "----\t"
    show' (Just x) = show x ++ "\t"

setField :: Int -> Int -> a -> Game2048 a -> Game2048 a
setField x y v = field . ix y . ix x ?~ v

-- getField :: Int -> Int -> Game2048 a -> Maybe a
-- getField x y = view (field . ix x . ix y)

rotate :: Game2048 a -> Game2048 a
rotate (Game xss) = Game (rotate' xss)
  where
    rotate' :: [[a]] -> [[a]]
    rotate' [] = []
    rotate' ([]:_) = []
    rotate' xss = reverse (map head xss) : rotate' (map tail xss) 

replenishLine :: Int -> [Maybe a] -> [Maybe a]
replenishLine n as = as ++ replicate (n - length as) Nothing

leftwards :: Game2048 a -> Game2048 a
leftwards (Game ass) = Game $ map (replenishLine 4 . map Just . catMaybes) ass

joinNums :: (Eq a, Num a) => Game2048 a -> (Game2048 a, a)
joinNums (Game ass) = (Game $ map (replenishLine 4) ass', sum pts)
  where
    (ass', pts) = unzip (map joinNums' ass)

    joinNums' :: (Eq a, Num a) => [Maybe a] -> ([Maybe a],a)
    joinNums' [] = ([],0)
    joinNums' [x] = ([x],0)
    joinNums' (Nothing:as) = joinNums' as
    joinNums' (Just x:Just y:as)
        | x == y = let (as',pt) = joinNums' as in (Just (x+y):as',x+y+pt)  
        | otherwise = let (as',pt) = joinNums' (Just y:as) in (Just x:as', pt)
    joinNums' (a:as) = let (as',pt) = joinNums' as in (a:as', pt)

data Direction
    = L
    | R
    | U
    | D
  deriving (Show, Eq)

ntimes :: Int -> (a -> a) -> a -> a
ntimes 0 _ = id
ntimes n f = f . ntimes (n-1) f

playerDraw :: (Eq a, Num a) => Direction -> Game2048 a -> (Game2048 a, a)
playerDraw dir = ntimes r rotate >>> leftwards >>> joinNums >>> (\(game, pts) -> (ntimes r' rotate game, pts))
  where
    r' = mod (-r) 4
    r = case dir of
        L -> 0
        D -> 1
        R -> 2
        U -> 3

possibleCPUDraws :: Game2048 a -> [(Int, Int)]
possibleCPUDraws (Game xss) =
    zip coords
    >>> map (\(y,xs) ->
        zipWith (\x f -> if isNothing f then Just x else Nothing) coords
        >>> catMaybes
        >>> map (\x -> (x,y))
        $ xs)
    >>> concat
    $ xss
  where
    coords :: [Int]
    coords = [0..]

possiblePlayerDraws :: (Num a, Eq a) => Game2048 a -> [Direction]
possiblePlayerDraws g = filter (\d -> let (g',_) = playerDraw d g in g' /= g) [L,R,U,D]


-------------------------------------------------------------------------------
-- Game Tree ------------------------------------------------------------------
-------------------------------------------------------------------------------

data GameTree a
    = GameOver a
    | Cut a
    | PlayerTurn a [(Direction, Int, GameTree a)]
    | CPUTurn a [(Point, GameTree a)]
  deriving (Show, Eq)

instance Functor GameTree where
    fmap f (GameOver a)      = GameOver $ f a 
    fmap f (PlayerTurn a as) = PlayerTurn (f a) [(d,pts,fmap f t) | (d,pts,t) <- as]
    fmap f (CPUTurn a as)    = CPUTurn (f a) [(d,fmap f t) | (d,t) <- as]

instance Comonad GameTree where
    extract (GameOver a)     = a
    extract (Cut a)          = a
    extract (PlayerTurn a _) = a
    extract (CPUTurn a _)    = a

    duplicate t@(GameOver _)      = GameOver t
    duplicate t@(Cut _)           = Cut t
    duplicate t@(PlayerTurn _ as) = PlayerTurn t [(d,pts,duplicate t) | (d,pts,t) <- as]
    duplicate t@(CPUTurn _ as)    = CPUTurn t [(d,duplicate t) | (d,t) <- as]


foldGameTree :: ()
    => (a -> result)
    -> (a -> result)
    -> (a -> [(Direction, Int, result)] -> result)
    -> (a -> [(Point, result)] -> result)
    -> GameTree a -> result
foldGameTree fGameOver fCut fPlayerTurn fCPUTurn = fld
  where
    fld (GameOver a)      = fGameOver a
    fld (Cut a)           = fCut a
    fld (PlayerTurn a as) = fPlayerTurn a [(d,pts,fld t) | (d,pts,t) <- as]
    fld (CPUTurn a as)    = fCPUTurn a [(d,fld t) | (d,t) <- as]


calculateGameTree :: GameTree (Game2048 Int)
calculateGameTree = cpuTurn newgame
  where
    cpuTurn :: Game2048 Int -> GameTree (Game2048 Int)
    cpuTurn game = case possibleCPUDraws game of
        [] -> GameOver game
        ps -> CPUTurn game [ (p,playerTurn game') | p@(x,y) <- ps, let game' = setField x y 2 game]

    playerTurn :: Game2048 Int -> GameTree (Game2048 Int)
    playerTurn game = case possiblePlayerDraws game of
        [] -> GameOver game
        ds -> PlayerTurn game [(d, pts, cpuTurn game') | d <- ds, let (game',pts) = playerDraw d game]


cutTree :: Int -> GameTree a -> GameTree a
cutTree 0 (PlayerTurn a _)  = Cut a
cutTree 0 (CPUTurn a _)     = Cut a
cutTree _ (GameOver a)      = GameOver a
cutTree _ (Cut a)           = Cut a
cutTree n (PlayerTurn a as) = PlayerTurn a [(d,p,cutTree (n-1) t) | (d,p,t) <- as]
cutTree n (CPUTurn a as)    = CPUTurn a [cutTree (n-1) <$> t | t <- as]

depth :: GameTree a -> Int
depth = foldGameTree
    (const 1)
    (const 1)
    (\_ as -> 1 + maximum (map snd3 as))
    (\_ as -> maximum (map snd as))

pp c a = do
    putStrLn c
    (ppGame2048 >>> putStrLn) a
    putStrLn ""

play :: (Show a, Ord ord) => Int -> (Game2048 a -> GameTree (Game2048 a) -> ord) -> GameTree (Game2048 a) -> IO (Game2048 a, Int)
play p _ (GameOver a) = pure (a,p)
play p _ (Cut a) = pure (a,p)
play p scoring (CPUTurn a as) = do
    (_, next) <- uniform as
    play p scoring next
play p scoring (PlayerTurn a as) = do
    pp ("(" ++ show p ++ "pts)") a
    let (_, p', next) = maximumBy (compare `on` (thr3 >>> scoring a)) as
    play (p+p') scoring next


playRand :: Show a => GameTree (Game2048 a) -> IO ()
playRand (GameOver a) = putStrLn . ppGame2048 $ a
playRand (Cut a) = putStrLn . ppGame2048 $ a
playRand (CPUTurn a as) = do
    pp "CPU" a
    (_, next) <- uniform as
    playRand next
playRand (PlayerTurn a as) = do
    pp "PLAYER" a
    (_, _, next) <- uniform as
    playRand next

pathLength :: Int -> GameTree a -> Int
pathLength cutDepth = cutTree cutDepth >>> depth

freeFields :: Game2048 a -> Int
freeFields = possibleCPUDraws >>> length

fieldGain' :: Game2048 a -> Game2048 a -> Int
fieldGain' a b = freeFields a - freeFields b

fieldGain :: Game2048 a -> GameTree (Game2048 a) -> Int
fieldGain game = fmap (fieldGain' game) >>> extract

probe :: StdGen -> Int -> Int -> GameTree (Game2048 a) -> GameTree (Game2048 a)
probe gen l b tree = evalRand (probe' l b tree) gen

probe' :: MonadRandom m => Int -> Int -> GameTree (Game2048 a) -> m (GameTree (Game2048 a))
probe' 0 _ tree = pure $ Cut (extract tree)
probe' _ _ g@(GameOver _) = pure g
probe' _ _ g@(Cut _) = pure g
probe' l b (CPUTurn a as) = do
    as' <- chooseN b as
    as'' <- forM as' $ \(p,a) -> do
        a' <- probe' (l-1) b a
        pure (p, a')
    pure $ CPUTurn a as''
probe' l b (PlayerTurn a as) = PlayerTurn a <$> mapM (\(d,p,t) -> do t' <- probe' l b t; pure (d,p,t')) as

chooseN :: MonadRandom m => Int -> [a] -> m [a]
chooseN n as = chooseN' (min n (length as)) as []
  where
    chooseN' 0  _ _ = pure []
    chooseN' n [] [] | n > 0 = error "Fucked..."
    chooseN' n [] ys = chooseN' n ys []
    chooseN' n (x:xs) ys = do
        die <- getRandomR ((0.0,1.0) :: (Double, Double))
        if die > 0.5 then do
            xs' <- chooseN' (n-1) xs ys
            pure $ x:xs'
        else do
            chooseN' n xs (x:ys)


chance :: GameTree (Game2048 a) -> Double
chance = foldGameTree
  (const 1.0)
  (const 0.0)
  (\_ as -> sum (map thr3 as) / (toEnum $ length as))
  (\_ as -> sum (map snd as) / (toEnum $ length as))

log2 :: Int -> Int
log2 = toEnum >>> logBase 2 >>> fromEnum

mostPoints :: GameTree (Game2048 Int) -> Int
mostPoints = foldGameTree
  (const 0)
  (const 0)
  (\_ -> map (\(_,pt1,pt2) -> log2 pt1+pt2) >>> sum)
  (\_ -> map snd >>> sum)

main :: IO ()
main = do
  gen <- getStdGen
  -- playRand calculateGameTree
  (game, pts) <- play 0 (\_ -> probe gen 5 2 >>> (\t -> toEnum (mostPoints t) * (1 - chance t))) calculateGameTree
  pp ("("++ show pts ++ "pts)Final Game State") game
