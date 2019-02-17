module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Monad as M
import qualified System.Console.ANSI as Ansi
import qualified System.IO as SysIO

import Lib


delayMilliseconds ms = threadDelay (ms * 1000)


gameLoop :: State -> IO ()
gameLoop state = do
    putStr $ show state
    Ansi.setCursorColumn 0
    SysIO.hFlush SysIO.stdout
    delayMilliseconds 50
    Ansi.clearLine
    gameLoop $ iterateState state


main :: IO ()
main = do
    Ansi.hideCursor
    gameLoop $ East 1
