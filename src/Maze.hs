module Maze where

import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.List.Index
import System.Random

data Grid = W | F
  deriving (Eq)
instance Show Grid where
  show W = "[]"
  show F = "  "
type XIndex = Int
type YIndex = Int
data Pos = Pos XIndex YIndex
instance Show Pos where
  show (Pos x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
instance Eq Pos where
  (Pos x1 y1) == (Pos x2 y2) = x1 == x2 && y1 == y2
newtype Board = Board [[Grid]]
instance Show Board where
  show (Board rows) = unlines $ map showRow rows
    where
      showRow = concatMap show

(!!!) :: Board -> Pos -> Grid
(Board board) !!! (Pos xIndex yIndex) = (board !! yIndex) !! xIndex

update :: Board -> Pos -> Grid -> Board
update (Board board) (Pos xIndex yIndex) grid = Board ( left yIndex board
                                                        ++ ( left xIndex row
                                                             ++ grid
                                                             : right xIndex row
                                                           )
                                                        : right yIndex board
                                                      )
  where
    left index = take index
    right index = drop (index + 1)
    row = board !! yIndex

expandRoad :: Int -> [a] -> [a]
expandRoad roadWidth =
  concat . imap (\index a -> if odd index
                               then replicate roadWidth a
                             else [a])

expandBoard :: Int -> Board -> Board
expandBoard roadWidth (Board board) = Board $ expandRoad roadWidth $ map (expandRoad roadWidth) board

roadWidth :: Int
roadWidth = 2

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

printBoard :: Board -> IO ()
printBoard board = do clearScreen
                      print $ expandBoard roadWidth board

makeMaze :: Int -> Board
makeMaze size = if odd size then Board [[W, F, W]] --until finished extendWall (initialBoard size)
                else error "size must be odd."
  where
    finished (Board board) = undefined

initialBoard :: Int -> Board
initialBoard size = undefined

extendWall :: MonadIO m => Board -> m Board
extendWall board = do startPos <- chooseStartPos board
                      liftIO $ putStr $ "startPos" ++ show startPos
                      (lastPos, pillarStack) <- iterateUntilM finished step (startPos, [])
                      liftIO $ putStr $ "lastPos, pillarStack" ++ show (lastPos, pillarStack)
                      return (foldl (\prevBoard pos -> update prevBoard pos W) board pillarStack)
  where
    chooseStartPos :: MonadIO m => Board -> m Pos
    chooseStartPos b = choose (getUnfinishedPos b)
    finished :: (Pos, [Pos]) -> Bool
    finished (pos, pillarStack) = all (`elem` pillarStack) (nextPillar pos)
    choosePillar :: MonadIO m => (Pos, [Pos]) -> m Pos
    choosePillar (pos, pillarStack) = choose $ filter (`notElem` pillarStack) $ nextPillar pos
    stack :: (Pos, [Pos]) -> Pos -> [Pos]
    stack (Pos x y, pillarStack) (Pos nextx nexty)
      | x == nextx && y /= nexty = Pos x nexty : (Pos x (y + ((nexty - y) `div` 2)) : pillarStack)
      | x /= nextx && y == nexty = Pos nextx y : (Pos (x + ((nextx - x) `div` 2)) y : pillarStack)
    step :: MonadIO m => (Pos, [Pos]) -> m (Pos, [Pos])
    step (pos, pillarStack) = do nextPos <- choosePillar (pos, pillarStack)
                                 let nextPillarStack = stack (pos, pillarStack) nextPos
                                 return (nextPos, nextPillarStack)

getUnfinishedPos :: Board -> [Pos]
getUnfinishedPos (Board board) = iconcatMap (\yIndex row -> [Pos xIndex yIndex | not (isEdge yIndex board), even yIndex, xIndex <- getUnfinishedPosRow row]) board
  where
    getUnfinishedPosRow grids = iconcatMap (\xIndex grid -> [xIndex | grid == F && not (isEdge xIndex grids) && even xIndex]) grids
    isEdge index as = index == 0 || index == length as - 1

nextPillar :: Pos -> [Pos]
nextPillar (Pos xIndex yIndex) = [ Pos (xIndex + 2) yIndex
                                 , Pos (xIndex - 2) yIndex
                                 , Pos xIndex (yIndex + 2)
                                 , Pos xIndex (yIndex - 2)
                                 ]

choose :: MonadIO m => [a] -> m a
choose as = do index <- randomRIO (0, end)
               return (as !! index)
  where
    end = length as - 1

sample = Board [ [W, W, W, W, W, W, W, W, W, W, W]
               , [W, F, W, W, W, F, W, F, W, F, W]
               , [W, W, W, W, W, W, W, W, W, W, W]
               , [W, F, W, F, W, F, W, F, W, F, W]
               , [W, W, W, W, W, W, W, W, W, W, W]
               , [W, F, W, F, W, F, W, F, W, F, W]
               , [W, W, W, W, W, W, W, W, W, W, W]
               , [W, F, W, F, W, F, W, F, W, F, W]
               , [W, W, W, W, W, W, W, W, W, W, W]
               , [W, F, W, F, W, F, W, F, W, F, W]
               , [W, W, W, W, W, W, W, W, W, W, W]
               ]

unfinished = Board [ [W, W, W, W, W, W, W, W, W]
                   , [W, F, F, F, F, F, F, F, W]
                   , [W, F, F, F, F, F, F, F, W]
                   , [W, F, F, F, F, F, F, F, W]
                   , [W, F, F, F, F, F, F, F, W]
                   , [W, F, F, F, F, F, F, F, W]
                   , [W, F, F, F, F, F, F, F, W]
                   , [W, F, F, F, F, F, F, F, W]
                   , [W, W, W, W, W, W, W, W, W]
                   ]