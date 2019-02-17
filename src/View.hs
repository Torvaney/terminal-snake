module View
    ( Square(..)
    , Row
    , Board
    , createBoard
    , modifyAt
    , updateBoard
    , view
    ) where

import qualified Data.List

import State
import Snake


data Square
    = SnakeHead Direction
    | SnakeBody
    | Apple
    | Empty
    deriving Show


squareChar :: Square -> Char
squareChar (SnakeHead North) = 'A'
squareChar (SnakeHead East)  = '>'
squareChar (SnakeHead South) = 'V'
squareChar (SnakeHead West)  = '<'
squareChar SnakeBody         = '#'
squareChar Apple             = '@'
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


getAt :: [a] -> Int -> Maybe a
getAt xs i
    | i < 0          = Nothing
    | length xs <= i = Nothing
    | otherwise      = Just (xs !! i)


updateBoard :: Square -> Coordinate -> Board -> Board
updateBoard square (x, y) board =
    case row of
        Just r ->
            modifyAt board y $
            modifyAt r x square
        Nothing ->
            board
    where row = getAt board y


createBoard :: State -> Board
createBoard state =
    updateBoard Apple appleCoord $
    updateBoard (SnakeHead headDirection) headCoord $
    foldl (flip (updateBoard SnakeBody)) initBoard bodyCoords
    where initBoard         = createEmptyBoard (cols state) (rows state)
          Body i bodyCoords = body (snake state)
          headDirection     = direction (snake state)
          headCoord         = coordinate (snake state)
          appleCoord        = apple state


-- NOTE: Could use Show here?
view :: State -> String
view state =
    Data.List.intercalate "\n" $
    map (map squareChar) $
    createBoard state
