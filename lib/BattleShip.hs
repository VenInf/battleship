{-# LANGUAGE RecordWildCards #-}
module BattleShip where

import Control.Monad.State
import Data.List (nub, sortOn)
import List.Shuffle
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.HashMap.Strict as M
import Debug.Trace

data ShootResult = Hit | Miss | HitSunk deriving (Show, Eq)

showShootResult :: ShootResult -> String
showShootResult Hit = "x"
showShootResult HitSunk = "#"
showShootResult Miss = "o"


data BoardResults = BoardResults { results :: [((Int, Int), ShootResult)]
                                 , n       :: Int -- on the horizontal axis
                                 , m       :: Int -- on the vertical axis
                                 } deriving (Show)

data BoardSecret = BoardSecret { board :: PlacedShips
                               , history :: [((Int, Int), ShootResult)]
                               } deriving (Show)

type ShootFn = Int -> Int -> State BoardSecret ShootResult

shootCords :: ShootFn
shootCords x y = do
    brdSecret <- get
    let hitShips = filter (`shipOverlapsPoint` (x, y)) (board brdSecret)
    case hitShips of
        [] -> do
            put $ brdSecret {history = ((x, y), Miss) : history brdSecret}
            pure Miss
        s:_ -> do
            let uncoveredCords = filter (`notElem` map fst (history brdSecret)) (shipToCords s)
            if uncoveredCords == [(x, y)]
            then do
                put $ brdSecret {history = ((x, y), HitSunk) : history brdSecret}
                pure HitSunk
            else do
                put $ brdSecret {history = ((x, y), Hit) : history brdSecret}
                pure Hit

unsunkShipsLengths :: BoardSecret -> [Int]
unsunkShipsLengths brdSecret = map shipLength unsunkShips
    where
        hitSunkCords = map fst $ filter ((== HitSunk) . snd) (history brdSecret)
        unsunkShips = filter (all (`notElem` hitSunkCords) . shipToCords) (board brdSecret)

sinkAllShips :: Int -> Int -> BoardSecret -> IO ()
sinkAllShips n m brdSecret = do
    let brdResults = BoardResults { results = [], n = n, m = m }
        shipLengths = unsunkShipsLengths brdSecret

    putStrLn "The secret map is "
    showBoardSecret brdSecret n m
    putStrLn ""

    shootLoop (brdSecret, brdResults) shipLengths


shootLoop :: (BoardSecret, BoardResults) -> [Int] -> IO ()
shootLoop (brdSecret, brdResults) shipLengths = do
    putStrLn "Current known map is "
    showBoardResults brdResults
    putStrLn ""

    mbResults <- shootHelper (brdSecret, brdResults) shipLengths
    case mbResults of
        Nothing -> pure ()
        Just (brdSecret', brdResults') ->
            if null (unsunkShipsLengths brdSecret')
            then do
                putStrLn "Sunk all ships"
                pure ()
            else shootLoop (brdSecret', brdResults') shipLengths


shootHelper :: (BoardSecret, BoardResults) -> [Int] -> IO (Maybe (BoardSecret, BoardResults))
shootHelper (brdSecret, brdResults) shipLengths = do
    mbBestCords <- bestCords brdResults shipLengths
    case mbBestCords of
        Nothing -> do
            putStrLn "No cords left"
            pure Nothing
        Just (x, y) -> do
            putStrLn $ "Shooting at " ++ show (x, y)
            let (res, brdSecret') = runState (shootCords x y) brdSecret
            putStrLn $ "Result " ++ show res
            let brdResults' = brdResults { results = ((x, y), res) : results brdResults}
            pure $ Just (brdSecret', brdResults')

-- From top left, x horizontal, y vertical
-- We assume that x1 <= x2 and y1 <= y2
data Ship = Ship { x1 :: Int
                 , y1 :: Int
                 , x2 :: Int
                 , y2 :: Int
                 } deriving (Show, Eq, Ord)

type PlacedShips = [Ship]

shipOverlapsPoint :: Ship -> (Int, Int) -> Bool
shipOverlapsPoint ship (x, y)
    | x1 ship == x && y1 ship <= y && y <= y2 ship = True
    | y1 ship == y && x1 ship <= x && x <= x2 ship = True
    | otherwise = False

shipOverlapsShip :: Ship -> Ship -> Bool
shipOverlapsShip ship1 ship2
    | y1 ship1 == y2 ship1
      && y1 ship2 <= y1 ship1 && y1 ship1 <= y2 ship2
      && x1 ship1 <= x1 ship2 && x1 ship2 <= x2 ship1 = True
    | y1 ship2 == y2 ship2
      && y1 ship1 <= y1 ship2 && y1 ship2 <= y2 ship1
      && x1 ship2 <= x1 ship1 && x1 ship1 <= x2 ship2 = True
    | otherwise = False

shipLength :: Ship -> Int
shipLength (Ship {..})
    | x1 == x2 = y2 - y1 + 1
    | y1 == y2 = x2 - x1 + 1
    | otherwise = error $ "Illegal Ship cords, " ++ show ((x1, y1), (x2, y2))

shipTouchesShip :: Ship -> Ship -> Bool
shipTouchesShip ship1 ship2 = or [ shipOverlapsShip ship1 (moveX 1 ship2)
                                 , shipOverlapsShip ship1 (moveX (-1) ship2)
                                 , shipOverlapsShip ship1 (moveY 1 ship2)
                                 , shipOverlapsShip ship1 (moveY (-1) ship2)
                                 ]
    where
        moveX dx ship = ship{x1 = x1 ship + dx, x2 = x2 ship + dx}
        moveY dy ship = ship{y1 = y1 ship + dy, y2 = y2 ship + dy}

isOnHit :: BoardResults -> Ship -> Bool
isOnHit board ship = any (shipOverlapsPoint ship) hitCords
    where
        hitCords = map fst $ filter ((`elem` [Hit, HitSunk]) . snd) (results board)

isOnMiss :: BoardResults -> Ship -> Bool
isOnMiss board ship = any (shipOverlapsPoint ship) missedCords
    where
        missedCords = map fst $ filter ((== Miss) . snd) (results board)

shipToCords :: Ship -> [(Int, Int)]
shipToCords (Ship {..})
    | x1 == x2 = [(x1, y) | y <- [y1 .. y2]]
    | y1 == y2 = [(x, y1) | x <- [x1 .. x2]]
    | otherwise = error $ "Illegal Ship cords, " ++ show ((x1, y1), (x2, y2))


boardShipPlacements :: BoardResults -> Int -> PlacedShips
boardShipPlacements board len = filter (not . isOnMiss board) legalShipPlacements
    where
        verticalOn x = [Ship {x1 = x, x2 = x, y1 = y, y2 = y + len - 1} | y <- [0 .. m board - len]]
        horizontalOn y = [Ship {x1 = x, x2 = x + len - 1, y1 = y, y2 = y} | x <- [0 .. n board - len]]

        legalShipPlacements = nub $ concat [verticalOn x <> horizontalOn y | x <- [0 .. n board - 1]
                                                                           , y <- [0 .. m board - 1]]


randomValidShipPlacements :: BoardResults -> [Int] -> IO [PlacedShips]
randomValidShipPlacements _ [] = pure []
randomValidShipPlacements board [l] = do
    shuffledPlacements <- shuffleIO $ boardShipPlacements board l
    pure $ map (:[]) shuffledPlacements

randomValidShipPlacements board (l:ls) = do
    shuffledPlacements <- shuffleIO $ boardShipPlacements board l -- One IO call per ship length is acceptable
    restPlaced <- randomValidShipPlacements board ls
    let potentialEntries = [(s, ps) | s <- shuffledPlacements, ps <- restPlaced]
        notTouchingEntries = filter (\e -> not (isOverlapping e || isTouching e)) potentialEntries

        placements = map (uncurry (:)) notTouchingEntries
        coveringHitsEntries = filter coversHits placements

    pure coveringHitsEntries
    where
        isOverlapping (s, ps) = any (shipOverlapsShip s) ps
        isTouching (s, ps) = any (shipTouchesShip s) ps

        coversHits :: [Ship] -> Bool
        coversHits ships = all coveredByShips hitCords
            where
                hitCords = map fst $ filter ((`elem` [Hit, HitSunk]) . snd) (results board)
                coveredByShips (x, y) = any (\ship -> shipOverlapsPoint ship (x, y)) ships

        coverHitSinkExactly :: [Ship] -> Bool
        coverHitSinkExactly = undefined

heatMap :: BoardResults -> [Int] -> Int -> IO (M.HashMap (Int, Int) Int)
heatMap board shipLengths sampleLimit = do
    shipPlacements <- randomValidShipPlacements board shipLengths
    let cordsPerSample = map (concatMap shipToCords) shipPlacements
        frequenciesPerSample = map giveFrequencies cordsPerSample

    pure $ foldr (M.unionWith (+)) M.empty (take sampleLimit frequenciesPerSample)
    where
        giveFrequencies :: [(Int, Int)] -> M.HashMap (Int, Int) Int
        giveFrequencies = foldr (\elmnt counterMap -> M.insertWith (+) elmnt 1 counterMap) M.empty

showHeatMap :: M.HashMap (Int, Int) Int -> BoardResults -> IO ()
showHeatMap hMap board = do
    let cords = [[(x, y) | x <- [0 .. n board - 1]] | y <- [0 .. m board - 1]]
    putStr $ unlines $ map showLine cords
    where
        showLine :: [(Int, Int)] -> String
        showLine crds = unwords $ map ((show . fromMaybe 0) . (`M.lookup` hMap)) crds

showBoardSecret :: BoardSecret -> Int -> Int -> IO ()
showBoardSecret brdSecret n m = do
    let cords = [[(x, y) | x <- [0 .. n - 1]] | y <- [0 .. m - 1]]
    putStr $ unlines $ map showLine cords
    where
        shipPoints = concatMap shipToCords (board brdSecret)
        showLine :: [(Int, Int)] -> String
        showLine crds = unwords $ map (\crd -> if crd `elem` shipPoints then "@" else "-" ) crds


showBoardResults :: BoardResults -> IO ()
showBoardResults brdResults = do
    let cords = [[(x, y) | x <- [0 .. n brdResults - 1]] | y <- [0 .. m brdResults - 1]]
    putStr $ unlines $ map showLine cords
    where
        knownPoints = results brdResults
        showLine :: [(Int, Int)] -> String
        showLine crds = unwords $ map (\crd -> maybe "-" showShootResult (lookup crd knownPoints)) crds


bestCords :: BoardResults -> [Int] -> IO (Maybe (Int, Int))
bestCords brd shipLengths = do -- TODO change to filtering out hit cords
        hMap <- heatMap brd shipLengths 1000 -- Number of random samples
        let frequenciesList = sortOn (negate . snd) $ M.toList hMap
            unmarked = filter (\(cords, _) -> cords `notElem` map fst (results brd)) frequenciesList

        pure $ fst <$> listToMaybe unmarked
