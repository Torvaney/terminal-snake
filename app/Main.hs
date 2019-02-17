module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Monad as M
import qualified System.Console.ANSI as Ansi
import qualified System.IO as SysIO

import Lib
import Board
import Snake


delayMilliseconds ms = threadDelay (ms * 1000)


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
    , cols   = 20
    , status = KeepPlaying
    }


clearBoard :: State -> IO ()
clearBoard state =
    M.replicateM_ (rows state - 1) (do
        Ansi.clearLine
        Ansi.cursorUp 1
        Ansi.setCursorColumn 0)


ifReadyDo :: SysIO.Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = SysIO.hReady hnd >>= f
    where f True = Just <$> x
          f _    = return Nothing


getUserInput :: IO a -> IO (Maybe a)
getUserInput _ = return Nothing


applyUserInput (Just _) state = state {snake = turn RightTurn (snake state)}
applyUserInput Nothing  state = state


gameLoop :: State -> IO ()
gameLoop state@State{status = GameOver} = do
    putStr $ drawBoard $ createBoard state
    putStrLn "\n\nGame Over!"
gameLoop state@State{status = _} = do
    putStr $ drawBoard $ createBoard state
    SysIO.hFlush SysIO.stdout
    delayMilliseconds 250
    clearBoard state
    p <- getUserInput getChar
    gameLoop $ next (applyUserInput p state)


main :: IO ()
main = do
    Ansi.hideCursor
    gameLoop initState
