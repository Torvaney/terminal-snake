module Snake
    ( Direction(..)
    , Body(..)
    , Coordinate
    , shift
    , extend
    , Snake(..)
    , slither
    , Turn(..)
    , turn
    ) where


-- DIRECTION

data Direction
    = North
    | West
    | South
    | East
    deriving (Eq, Show)


-- Can I not just do this with typeclasses?
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


move :: Direction -> Coordinate -> Coordinate
move North (x, y) = (x, y - 1)
move West  (x, y) = (x - 1, y)
move South (x, y) = (x, y + 1)
move East  (x, y) = (x + 1, y)


-- BODY

data Body a = Body Int [a] deriving (Show)

-- instance Foldable Body where
--     foldr f z (Body i xs) = Body i (foldr f z xs)


shift :: Body a -> a -> Body a
shift (Body i xs) x = Body i $ take i $ x : xs


extend :: Body a -> a -> Body a
extend (Body i xs) x = Body (i + 1) $ x : xs


-- SNAKE

data Snake = Snake
    { direction  :: Direction
    , coordinate :: Coordinate
    , body       :: Body Coordinate
    } deriving Show


slither :: Snake -> Snake
slither snake = snake { coordinate = move (direction snake) (coordinate snake)
                      , body       = shift (body snake) (coordinate snake)
                      }


data Turn
    = LeftTurn
    | RightTurn
    deriving (Eq, Show)


turn :: Turn -> Snake -> Snake
turn LeftTurn  snake = snake {direction = turnLeft  (direction snake)}
turn RightTurn snake = snake {direction = turnRight (direction snake)}
