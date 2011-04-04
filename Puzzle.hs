module Main

where

import Control.Monad

import qualified Data.List as LIST
import qualified Data.Map as MAP
import qualified Data.Maybe as MAYBE
import qualified Data.Set as SET


data Height = ONE | TWO | THREE | FOUR | FIVE | FIVE_PLUS | SIX_MINUS | SIX
  deriving (Show, Eq, Ord, Enum)

data Color = RED | GREEN | BLUE | YELLOW | ORANGE | PURPLE
  deriving (Show, Eq, Ord, Enum)

data Brick = Brick {
    bHeight :: ! Height
  , bColor  :: ! Color
  }
  deriving (Show, Eq)

data Space = Space {
    sHeight :: ! Height
  , sBrick  :: ! (Maybe Brick)
  , sColors :: ! (SET.Set Color)
  }
  deriving (Show)

type Coord = (Int,Int)
type Field = MAP.Map Coord Space
type Bank = MAP.Map Height (SET.Set Color)
type Solution = MAP.Map Coord Brick

emptySpace :: Height -> Space
emptySpace h = Space h Nothing $ SET.fromList allColors

emptyField :: Field
emptyField = MAP.map emptySpace $ MAP.fromList $ zip allCoords heights
  where
  heights = [SIX,   THREE,     TWO,   FOUR,      FIVE,  ONE
            ,FOUR,  FIVE,      ONE,   SIX,       THREE, TWO
            ,ONE,   TWO,       FOUR,  THREE,     SIX,   FIVE
            ,TWO,   FIVE_PLUS, THREE, SIX_MINUS, ONE,   FOUR
            ,THREE, ONE,       FIVE,  TWO,       FOUR,  SIX
            ,FIVE,  FOUR,      SIX,   ONE,       TWO,   THREE]

fullBank :: Bank
fullBank = foldl (\m h -> MAP.insert h (availableColors h) m) MAP.empty allHeights
  where
  availableColors height
    | height == FIVE      = SET.fromList $ filter (/=YELLOW) allColors
    | height == FIVE_PLUS = SET.singleton YELLOW
    | height == SIX_MINUS = SET.singleton ORANGE
    | height == SIX       = SET.fromList $ filter (/=ORANGE) allColors
    | otherwise           = SET.fromList allColors
  
allIndices :: [Int]
allIndices = [1..6]

allCoords :: [(Int,Int)]
allCoords = [(x,y)|y<-allIndices,x<-allIndices]

allColors :: [Color]
allColors = enumFrom RED

allHeights :: [Height]
allHeights = enumFrom ONE

newPuzzle :: (Bank,Field)
newPuzzle = (fullBank, emptyField)

getHeightAtPosition :: Coord -> Field -> Maybe Height
getHeightAtPosition coord field
  = do
    space <- MAP.lookup coord field
    return $ sHeight space

takeBrickFromBank :: Height -> Color -> Bank -> Maybe (Bank, Brick)
takeBrickFromBank height color bank
  = do
    colors <- MAP.lookup height bank
    if SET.member color colors
      then do
        let colors' = SET.delete color colors
            bank'   = MAP.insert height colors' bank
            brick   = Brick height color
        return (bank', brick)
      else Nothing

putBrickToField :: Coord -> Brick -> Field -> Maybe Field
putBrickToField coord@(col,row) brick field
  = do
    space <- MAP.lookup coord field
    guard $ MAYBE.isNothing $ sBrick space
    guard $ SET.member color (sColors space)
    let space' = space { sBrick = Just brick, sColors = SET.empty}
        field' = updateColors $ MAP.insert coord space' field
    return field'
    where
    color = bColor brick
    xyCoords
      = [(col,y)|y<-allIndices]++[(x,row)|x<-allIndices]
    deleteColorFromSpace space
      = space { sColors = SET.delete color $ sColors space }
    deleteColorFromField f c
      = MAP.update (Just . deleteColorFromSpace) c f
    updateColors f = foldl deleteColorFromField f xyCoords 

checkRowColors :: Int -> Field -> Bool
checkRowColors row = checkCoordColors [(x,row)|x<-allIndices]

checkColColors :: Int -> Field -> Bool
checkColColors col = checkCoordColors [(col,y)|y<-allIndices]

checkCoordColors :: [Coord] -> Field -> Bool
checkCoordColors coords field = not $ any (>1) $ MAP.elems countMap
  where
  spaces = MAYBE.mapMaybe (`MAP.lookup` field) coords
  bricks = MAYBE.mapMaybe sBrick spaces
  colors = map bColor bricks
  countMap = foldl (flip $ MAP.alter $ Just . MAYBE.maybe (1::Int) (+1)) MAP.empty colors
  
doMove :: (Int,Int) -> Color -> (Bank, Field) -> Maybe (Bank,Field)
doMove coord@(col,row) color (bank, field)
  = do
    height <- getHeightAtPosition coord field
    (bank',brick)  <- takeBrickFromBank height color bank
    field' <- putBrickToField coord brick field
    guard $ checkRowColors row field'
    guard $ checkColColors col field'
    return (bank', field')

getSolutionFromField :: Field -> Solution
getSolutionFromField = MAP.map (MAYBE.fromJust . sBrick)

solvePuzzle :: [Coord] -> (Bank, Field) -> [Solution] -> [Solution]
solvePuzzle [] (_,field) solutions = solutions ++ [getSolutionFromField field]
solvePuzzle (coord:cs) puzzle solutions = solutions ++ solutions'
  where
  puzzles' = MAYBE.mapMaybe (\color -> doMove coord color puzzle) allColors
  solutions' = concatMap (\p -> solvePuzzle cs p []) puzzles'

prettyPrintSolution :: Solution -> String
prettyPrintSolution solution = LIST.unlines $ map prettyPrintRow allIndices
  where
  rowCoords rn = [(x,rn)|x<-allIndices]
  getBricks cs = map (\c -> MAYBE.fromJust $ MAP.lookup c solution) cs
  prettyPrintRow rn = LIST.unwords $ map prettyPrintBrick $ getBricks $ rowCoords rn
  prettyPrintBrick brick
    = (prettyPrintColor $ bColor brick) ++ "-" ++ (prettyPrintHeight $ bHeight brick)
  prettyPrintColor color = case color of
    RED -> "r"
    GREEN -> "g"
    BLUE -> "b"
    YELLOW -> "y"
    ORANGE -> "o"
    PURPLE -> "p"
  prettyPrintHeight height = case height of
    ONE -> "1"
    TWO -> "2"
    THREE -> "3"
    FOUR -> "4"
    FIVE -> "5"
    FIVE_PLUS -> "5"
    SIX_MINUS -> "6"
    SIX -> "6"

main :: IO ()
main
  = do
    let solutions = solvePuzzle allCoords newPuzzle []
    putStrLn $ prettyPrintSolution $ head solutions
    -- putStrLn $ LIST.unlines $ map prettyPrintSolution solutions
    -- putStrLn $ show $ length solutions
