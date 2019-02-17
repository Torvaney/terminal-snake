module Lib
    ( Status(..)
    , State(..)
    , next
    ) where


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
    if (x >= c) || (y >= r)
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


checkState :: State -> State
checkState state =
    checkOuroboros $ checkLimits state


next :: Maybe Action -> State -> State
next (Just action) state = checkState $ state { snake = slither action (snake state) }
next Nothing       state = state
