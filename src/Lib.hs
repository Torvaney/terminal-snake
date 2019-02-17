module Lib
    ( State(..)
    , iterateState
    ) where


data State
    = East Int
    | West Int
    deriving (Eq, Ord)


instance Show State where
    show (East i) = replicate (i - 1) '#' ++ ">"
    show (West i) = replicate (i - 1) '#' ++ "<"


iterateState :: State -> State
iterateState (East i) | i >= 59 = West 60
                      | otherwise = East (i + 1)
iterateState (West i) | i <= 2 = East 1
                      | otherwise = West (i - 1)
