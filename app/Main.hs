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


parseDirection :: Char -> Maybe Turn
parseDirection c = case c of
    's' -> Just LeftTurn
    'd' -> Just RightTurn
    _   -> Nothing


getCharImmediately :: IO Char
getCharImmediately = do
    hSetBuffering stdin NoBuffering
    getChar


getDirections :: Producer (Maybe Turn) IO ()
getDirections = M.forever $ do
    c <- lift getCharImmediately
    yield $ parseDirection c


ticktock :: Producer (Maybe Turn) IO ()
ticktock = M.forever $ do
    lift $ delayMilliseconds 250
    yield Nothing


applyUserAction :: State -> Maybe Turn -> State
applyUserAction state event =
    case event of
        Just thatWay -> Lib.next $ state {snake = turn thatWay (snake state)}
        Nothing      -> Lib.next state


consumeUserActions :: State -> Consumer (Maybe Turn) IO ()
consumeUserActions state = do
    event <- await
    let newState = applyUserAction state event
    lift $ clearBoard newState
    lift $ putStr $ drawBoard $ createBoard newState
    lift $ hFlush stdout
    let nextState = Lib.next newState
    case status state of
        GameOver -> lift $ putStrLn "\n\nGame Over!\n"
        _        -> consumeUserActions nextState


main :: IO ()
main = do
    putStrLn $ drawBoard $ createBoard initState
    (output, input) <- spawn unbounded

    forkIO $ do runEffect $ ticktock >-> toOutput output
                performGC
    forkIO $ do runEffect $ getDirections >-> toOutput output
                performGC

    runEffect $ fromInput input >-> consumeUserActions initState
