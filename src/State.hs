module State
    ( Status(..)
    , State(..)
    , next
    ) where

import System.Random (randomRIO)

import Snake


data Status
    = KeepPlaying
    | GameOver


data State = State
    { snake  :: Snake
    , rows   :: Int
    , cols   :: Int
    , status :: Status
    , apple  :: Coordinate
    }


checkLimits :: State -> State
checkLimits state =
    if (x >= c) || (y >= r) || (x < 0) || (y < 0)
        then state {status = GameOver}
        else state
    where c      = cols state
          r      = rows state
          (x, y) = coordinate $ snake state


checkOuroboros :: State -> State
checkOuroboros state =
    if coord `elem` coords
        then state {status = GameOver}
        else state
    where coord         = coordinate $ snake state
          Body _ coords = body $ snake state


randomCoord :: State -> IO Coordinate
randomCoord state = (,) <$> x <*> y
    where x = randomRIO (0, cols state - 1)
          y = randomRIO (0, rows state - 1)


updateApple :: State -> Coordinate -> State
updateApple state newApple =
    if snakeCoord == appleCoord
        then state { snake = (snake state) {body = extend snakeBody}
                   , apple = newApple
                   }
        else state
    where snakeCoord = coordinate $ snake state
          appleCoord = apple state
          snakeBody  = body $ snake state


checkApple :: State -> IO State
checkApple state =
    updateApple state <$> newApple
    where newApple = randomCoord state


checkState :: State -> IO State
checkState = checkApple . checkOuroboros . checkLimits


next :: Maybe Action -> State -> IO State
next (Just action) state = checkState $ state { snake = slither action (snake state) }
next Nothing       state = pure state
