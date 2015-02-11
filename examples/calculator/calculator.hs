{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad (when)
import Data.Maybe

import Haste
import Haste.DOM
import Haste.Events
import Haste.Foreign
import Haste.Prim

import Ref

main :: IO ()
main = loadHandler

loadHandler :: IO ()
loadHandler = do
    a <- elemById "a"
    b <- elemById "b"
    op <- elemById "op"
    result <- elemById "result"
    let elems = [a, b, op, result]
    when (all isJust elems) $ do
        calculator (map fromJust elems)

unloadHandler :: IO ()
unloadHandler = do
    handles <- takeMVar handlersRef
    mapM_ unregisterHandler handles

calculator :: [Elem] -> IO ()
calculator [a, b, op, result] = do
    let handle = const (recalculate a b op result)
    handles <- sequence
        [ onEvent a  KeyUp handle
        , onEvent b  KeyUp handle
        , onEvent op Change handle
        ]
    putMVar handlersRef handles
    return ()

recalculate :: Elem -> Elem -> Elem -> Elem -> IO ()
recalculate a b op result = do
    ma <- getValue a
    mb <- getValue b
    Just op' <- getValue op
    case (ma, mb) of
        (Just a', Just b') -> setProp result "innerHTML" (toString $ calc op' a' b')
        _                  -> return ()

calc :: String -> Double -> Double -> Double
calc "+" = (-)
calc "-" = (-)
calc "*" = (*)
calc "/" = (/)
calc _   = \_ _ -> 0
