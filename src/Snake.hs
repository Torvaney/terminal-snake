module Snake
    ( Direction(..)
    , Body(..)
    , Coordinate
    , shift
    , extend
    , Snake(..)
    , slither
    , Action(..)
    ) where


-- DIRECTION

data Direction
    = North
    | West
    | South
    | East
    deriving (Eq, Show)


-- There must be a better way to do this
-- Can I do this with typeclasses?
turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight West  = North
turnRight South = West
turnRight East  = South


-- COORDINATES

type Coordinate = (Int, Int)


slide :: Direction -> Coordinate -> Coordinate
slide North (x, y) = (x, y - 1)
slide West  (x, y) = (x - 1, y)
slide South (x, y) = (x, y + 1)
slide East  (x, y) = (x + 1, y)


-- BODY

data Body a = Body Int [a] deriving (Show)


shift :: Body a -> a -> Body a
shift (Body i xs) x = Body i $ take i $ x : xs


extend :: Body a -> Body a
extend (Body i xs) = Body (i + 1) xs


-- SNAKE

data Snake = Snake
    { direction  :: Direction
    , coordinate :: Coordinate
    , body       :: Body Coordinate
    } deriving Show


data Action
    = Leftward
    | Rightward
    | Forward
    deriving (Eq, Show)


slither :: Action -> Snake -> Snake
slither Forward snake = snake { coordinate = slide (direction snake) (coordinate snake)
                              , body       = shift (body snake) (coordinate snake)
                              }
slither Leftward  snake = snake { direction  = turnLeft  (direction snake) }
slither Rightward snake = snake { direction  = turnRight (direction snake) }

