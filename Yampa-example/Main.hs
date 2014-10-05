{-# LANGUAGE Arrows #-}

module
    Main
where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Data.Functor ((<$))

import qualified FRP.Yampa as P
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import Data.IORef

import Buttons


theMachine :: P.SF (P.Event G.Event) Picture
theMachine = P.loopPre (True, True, True) go
  where
    go = proc (e, (mode0, mode5, mode10)) ->
      do
        let
            click0 = ((Just Click ==) . filter0) `P.filterE` e
            click5 = ((Just Click ==) . filter5) `P.filterE` e
            click10 = ((Just Click ==) . filter10) `P.filterE` e
    
            toggle0 = ((Just Toggle ==) . filter0) `P.filterE` e
            toggle5 = ((Just Toggle ==) . filter5) `P.filterE` e
            toggle10 = ((Just Toggle ==) . filter10) `P.filterE` e


        -- ---------------------------
        -- Higher order implementation
        -- ---------------------------
    
        -- Every toggle event causes switch of counters, with every counter is newly created.
        -- This is NOT EXACTLY the required implementation.
        let newCounter0 = if mode0 then arr $ const (-1) else counter
        show0 <- P.rSwitch counter -< (click0, newCounter0 <$ toggle0)
    
        -- Every toggle event causes switch of a counter, with one counter reused.
        -- This is EXACTLY the required implementation.
        show5 <- 
            (let 
                active pa _ = 
                    P.kSwitch 
                        pa 
                        (arr $ \((_, mode), _) -> (==True) `P.filterE` mode) 
                        inactive
                inactive pa _ = 
                    P.switch 
                        (arr (const (-1)) *** (arr $ ((==False) `P.filterE`)))
                        (active pa)
              in
                active (arr fst >>> counter) True)
            -< (click5, mode5 <$ toggle5)
    
        -- A counter is always active, while the downstream switches every toggle event.
        -- This is the nonsuitable trivial implementation described at README.md.
        let newTee = if mode10 then arr $ const (-1) else id
        count10 <- counter -< click10
        show10 <- P.rSwitch id -< (count10, newTee <$ toggle10)
    
    
        returnA -< (renderButtons show0 show5 show10, (show0 >= 0, show5 >= 0, show10 >= 0))

    counter = proc e -> P.accumHold 0 -< (+1) <$ e




main :: IO ()
main =
  do
    vPic <- newIORef $ renderButtons 0 0 0

    handle <- P.reactInit 
        (return P.NoEvent)
        (\_ changed pic -> 
          do
            if changed then vPic `writeIORef` pic else return ()
            return False)
        theMachine

    G.playIO
        (InWindow "Yampa Example" (320, 240) (800, 200))
        white
        300
        ()
        (const $ readIORef vPic)
        (\e _ -> P.react handle (1.0, Just (P.Event e)) >> return ())
        (\_ w -> return w)
