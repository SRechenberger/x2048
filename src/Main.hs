{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Comonad
import Control.Monad (forM)
import Control.Concurrent

import Control.Monad.Random (MonadRandom, uniform, evalRand, getRandomR)
import System.Random (StdGen, getStdGen)
import System.IO (stdout, hSetBuffering, BufferMode (..))

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

--log2 :: Int -> Int
--log2 = toEnum >>> logBase 2 >>> fromEnum

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

coeff2 :: [(Double,Double)]
coeff2 = (,) <$> coeff <*> coeff

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

joinNums :: (Ord a, Num a) => Game2048 a -> (Game2048 a, a)
joinNums (Game ass) = (Game $ map (replenishLine 4) ass', sum pts)
  where
    (ass', pts) = unzip (map joinNums' ass)

    joinNums' :: (Ord a, Num a) => [Maybe a] -> ([Maybe a],a)
    joinNums' [] = ([],0)
    joinNums' [x] = ([x],0)
    joinNums' (Nothing:as) = joinNums' as
    joinNums' (Just x:Just y:as)
        | x == y = let (as',pt) = joinNums' as in (Just (x+y):as',(x+y)+pt)  
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

playerDraw :: (Ord a, Num a) => Direction -> Game2048 a -> (Game2048 a, a)
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

possiblePlayerDraws :: (Num a, Ord a) => Game2048 a -> [Direction]
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
{-
    duplicate t@(GameOver _)      = GameOver t
    duplicate t@(Cut _)           = Cut t
    duplicate t@(PlayerTurn _ as) = PlayerTurn t [(d,pts,duplicate t) | (d,pts,t) <- as]
    duplicate t@(CPUTurn _ as)    = CPUTurn t [(d,duplicate t) | (d,t) <- as]
-}
    extend f t@(GameOver _) = GameOver (f t)
    extend f t@(Cut _) = Cut (f t)
    extend f t@(PlayerTurn _ as) = PlayerTurn (f t) [(d,pts,extend f t) | (d,pts,t) <- as]
    extend f t@(CPUTurn _ as) = CPUTurn (f t) [(p,extend f t) | (p,t) <- as]


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

data Player = PLAYER | CPU deriving (Eq, Show)

simulateGameTree :: (Num a, Ord a, Show a)
    => (Game2048 a -> result)
    -> (Game2048 a -> result)
    -> (Game2048 a -> [(Direction, a, result)] -> result)
    -> (Game2048 a -> [(Point, result)] -> result)
    -> Player
    -> Int
    -> Game2048 a
    -> IO result
simulateGameTree fGameOver fCut fPlayerTurn fCPUTurn = sim
  where
    sim _ 0 g = pure $ fCut g
    sim PLAYER n g = case possiblePlayerDraws g of
        [] -> pure $ fGameOver g 
        ds -> do
            let nexts = [(d,pts,g') | d <- ds, let (g', pts) = playerDraw d g]
            mvars <- forM nexts $ \(d,pts,g') -> do
                mvar <- newEmptyMVar
                forkIO $ do
                    r <- sim CPU (n-1) g'
                    putMVar mvar (d,pts,r)
                pure mvar
            fPlayerTurn g <$> mapM readMVar mvars
    sim CPU n g = case possibleCPUDraws g of
        [] -> pure $ fGameOver g
        ps -> do
            let nexts = [(p,g') | p@(x,y) <- ps, let g' = setField x y 2 g]
            mvars <- forM nexts $ \(d,g') -> do
                mvar <- newEmptyMVar
                forkIO $ do
                    r <- sim PLAYER (n-1) g'
                    putMVar mvar (d,r)
                pure mvar
            fCPUTurn g <$> mapM readMVar mvars



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

play :: (Show ord, Show a, Ord ord)
     => Int
     -> (Game2048 a -> GameTree (Game2048 a) -> ord)
     -> GameTree (Game2048 a)
     -> IO (Game2048 a, Int)
play p _ (GameOver a) = pure (a,p)
play p _ (Cut a) = pure (a,p)
play p scoring (CPUTurn a as) = do
    (_, next) <- uniform as
    play p scoring next
play p scoring t@(PlayerTurn a as) = do
    -- pp ("(points: " ++ show p ++ ")") a
    putStr $ "           " ++ show p ++ " pts\r"
    let (_, p', next) = maximumBy (compare `on` (thr3 >>> scoring a)) as
    play (p+p') scoring next

play' :: Player -> (Game2048 Int -> IO Double) -> Int -> Game2048 Int -> IO (Game2048 Int, Int)
play' CPU scoring pts g = case possibleCPUDraws g of
    [] -> pure (g,pts)
    ds -> do
        (x,y) <- uniform ds
        play' PLAYER scoring pts (setField x y 2 g)
play' PLAYER scoring pts g = case possiblePlayerDraws g of
    [] -> pure (g,pts)
    ds -> do
        putStr $ "           " ++ show pts ++ " pts\r"
        scores <- forM [(d,pts,g') | d <- ds, let (g',pts) = playerDraw d g] $ \(d,pts,g') -> do
            score <- scoring g'
            pure (d,pts,score)
        let (d,pts',_) = maximumBy (compare `on` thr3) scores
        play' CPU scoring (pts' + pts) (fst $ playerDraw d g)


playRand :: Show a => GameTree (Game2048 a) -> IO ()
playRand (GameOver a) = putStrLn . ppGame2048 $ a
playRand (Cut a) = putStrLn . ppGame2048 $ a
playRand (CPUTurn a as) = do
    pp "CPU" a
    (_, next) <- uniform as
    playRand next
playRand t@(PlayerTurn a as) = do
    pp ("PLAYER") a
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

chance' :: Game2048 Int -> IO Double
chance' = simulateGameTree
  (const 1.0)
  (const 0.0)
  (\_ as -> sum (map thr3 as) / (toEnum $ length as))
  (\_ as -> sum (map snd as) / (toEnum $ length as))
  PLAYER
  5

mostPoints :: GameTree (Game2048 Int) -> Int
mostPoints = foldGameTree
  (const 0)
  (const 0)
  (\_ -> map (on2 (+) snd3 thr3) >>> maximum)
  (\_ -> map snd >>> minimum)

mostPoints' :: Game2048 Int -> IO Int
mostPoints' = simulateGameTree
  (const 0)
  (const 0)
  (\_ -> map (on2 (+) snd3 thr3) >>> maximum)
  (\_ -> map snd >>> minimum)
  PLAYER
  5    

score :: Double -> Double -> (GameTree (Game2048 Int)) -> Double
score a b t = a * pts * chanceSuccess + b * pts * ff
  where
    pts = toEnum (mostPoints t)
    ff  = toEnum (freeFields (extract t))
    chanceSuccess = (1 - chance t)

score' :: Double -> Double -> (Game2048 Int) -> IO Double
score' a b g = do
    pts <- toEnum <$> mostPoints' g
    chanceSuccess <- (1 -) <$> chance' g
    pure $ a * pts * chanceSuccess + b * pts * ff
  where
    ff  = toEnum (freeFields g)

main :: IO ()
main = do
  gen <- getStdGen
  hSetBuffering stdout NoBuffering 
  let tree = calculateGameTree
  results <- forM coeff2 $ \c@(a,b) -> do
    putStrLn $ "a = " ++ show a ++ " b = " ++ show b
    result <- play' CPU (score' a b) 0 newgame
    putStrLn $ "  finally: " ++ show (snd result)
    pure (c,result)
  
  let (c,(game,pts)) = maximumBy (compare `on` (snd >>> snd)) results
  putStrLn $ "Best Result: " ++ show c ++ " " ++ show c ++ " pts\n" ++ ppGame2048 game ++ "\a\n"
