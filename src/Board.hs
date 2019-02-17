module Board
    ( Square(..)
    , Row
    , Board
    , createBoard
    , modifyAt
    , updateBoard
    , drawBoard
    ) where

import qualified Data.List

import Lib
import Snake


data Square
    = SnakeHead Direction
    | SnakeBody
    | Empty
    deriving Show


squareChar :: Square -> Char
squareChar (SnakeHead North) = 'A'
squareChar (SnakeHead East)  = '>'
squareChar (SnakeHead South) = 'V'
squareChar (SnakeHead West)  = '<'
squareChar SnakeBody         = '#'
squareChar Empty             = '•'


type Row = [Square]

type Board = [Row]


createEmptyBoard :: Int -> Int -> Board
createEmptyBoard x y =
    replicate y $
    replicate x Empty


modifyAt :: [a] -> Int -> a -> [a]
modifyAt xs i x =
    take i xs ++ [x] ++ drop (i+1) xs


updateBoard :: Square -> Coordinate -> Board -> Board
updateBoard square (x, y) board =
    modifyAt board y $
    modifyAt row x square
    where row = board !! y


createBoard :: State -> Board
createBoard state =
    updateBoard (SnakeHead headDirection) headCoord $
    foldl (flip (updateBoard SnakeBody)) initBoard bodyCoords
    where initBoard         = createEmptyBoard (cols state) (rows state)
          Body i bodyCoords = body (snake state)
          headDirection     = direction (snake state)
          headCoord         = coordinate (snake state)


drawBoard :: Board -> String
drawBoard board =
    Data.List.intercalate "\n" $
    map (map squareChar) board
