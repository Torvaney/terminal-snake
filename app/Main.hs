module Main where

import qualified Control.Concurrent as CC
import qualified Control.Monad as M
import qualified System.Console.ANSI as Ansi
import Control.Exception (bracket)
import System.IO

import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P

import Snake
import State
import View


-- INIT GAME

startX = 10
startY = 10
startPosition = (startX, startY)


initSnake :: Snake
initSnake = Snake
    { direction  = East
    , coordinate = startPosition
    , body       = Body 2 []
    }


initState :: State
initState = State
    { snake  = initSnake
    , rows   = 20
    , cols   = 50
    , status = KeepPlaying
    , apple  = (startX + 1, startY)  -- Sneaky hack to put the Apple somewhere random
    }


-- RENDERING

clearView :: State -> IO ()
clearView state =
    M.replicateM_ (rows state - 1) (do
        Ansi.clearLine
        Ansi.cursorUp 1
        Ansi.setCursorColumn 0)


-- USER INPUT

delayMilliseconds ms = CC.threadDelay (ms * 1000)


parseDirection :: Char -> Maybe Action
parseDirection c = case c of
    's' -> Just Leftward
    'd' -> Just Rightward
    _   -> Nothing


getCharImmediately :: IO Char
getCharImmediately = do
    hSetBuffering stdin NoBuffering
    getChar


getDirections :: Producer (Maybe Action) IO ()
getDirections = M.forever $ do
    c <- lift getCharImmediately
    yield $ parseDirection c


ticktock :: Producer (Maybe Action) IO ()
ticktock = M.forever $ do
    lift $ delayMilliseconds 150  -- Controls snake speed (ms / step)
    yield $ Just Forward


consumeUserActions :: State -> Consumer (Maybe Action) IO ()
consumeUserActions state = do
    action <- await
    newState <- lift $ State.next action state
    lift $ clearView newState
    lift $ putStr $ view newState
    lift $ hFlush stdout
    case status state of
        GameOver -> lift $ putStrLn "\n\nGame Over!\n"
        _        -> consumeUserActions newState


main :: IO ()
main = do
    putStr $ view initState
    (output, input) <- spawn unbounded

    forkIO $ do runEffect $ ticktock >-> toOutput output
                performGC
    forkIO $ do runEffect $ getDirections >-> toOutput output
                performGC

    runEffect $ fromInput input >-> consumeUserActions initState
