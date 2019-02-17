module Main where

import qualified Control.Concurrent as CC
import qualified Control.Monad as M
import qualified System.Console.ANSI as Ansi
import Control.Exception (bracket)
import System.IO

import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P

import Lib
import Board
import Snake


-- INIT GAME

initSnake :: Snake
initSnake = Snake
    { direction  = East
    , coordinate = (10, 10)
    , body       = Body 3 []
    }


initState :: State
initState = State
    { snake  = initSnake
    , rows   = 20
    , cols   = 50
    , status = KeepPlaying
    , apple  = (3, 2)
    }


-- RENDERING

clearBoard :: State -> IO ()
clearBoard state =
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
    lift $ delayMilliseconds 250
    yield $ Just Forward


consumeUserActions :: State -> Consumer (Maybe Action) IO ()
consumeUserActions state = do
    action <- await
    let newState = Lib.next action state
    lift $ clearBoard newState
    lift $ putStr $ drawBoard $ createBoard newState
    lift $ hFlush stdout
    case status state of
        GameOver -> lift $ putStrLn "\n\nGame Over!\n"
        _        -> consumeUserActions newState


main :: IO ()
main = do
    putStrLn $ drawBoard $ createBoard initState
    (output, input) <- spawn unbounded

    forkIO $ do runEffect $ ticktock >-> toOutput output
                performGC
    forkIO $ do runEffect $ getDirections >-> toOutput output
                performGC

    runEffect $ fromInput input >-> consumeUserActions initState
