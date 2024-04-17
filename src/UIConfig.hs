{-# LANGUAGE UnicodeSyntax, Rank2Types #-}

module UIConfig (playBanana, InputEvent) where

import Graphics.Gloss hiding (display)
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Exit

type InputEvent = G.Event

playBanana
    :: Display 
    -> Color   
    -> Int     
    -> (Event InputEvent -> MomentIO (Behavior Picture))
    -> IO ()
playBanana display colour fps mainBanana = do
    picRef                    <- newIORef blank
    (eventHandler, fireEvent) <- newAddHandler
    let handleEvent e@(G.EventKey k G.Down _ _) = case k of
            (G.SpecialKey G.KeyEsc) -> exitSuccess
            _                       -> fireEvent e
        handleEvent e = fireEvent e

    network <- compile $ do
        glossEvent <- fromAddHandler eventHandler
        picture    <- mainBanana glossEvent
        changes picture >>= reactimate' . fmap (fmap (writeIORef picRef))
        valueBLater picture >>= liftIO . writeIORef picRef
    actuate network

    playIO display
           colour
           fps
           ()
           (\() -> readIORef picRef)
           (\e () -> handleEvent e)
           (\_ () -> pure ())


