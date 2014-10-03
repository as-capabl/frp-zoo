{-# LANGUAGE Arrows #-}

module
    Main
where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Data.Functor ((<$))

import qualified Control.Arrow.Machine as P
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import Buttons

type MainProc = P.ProcessA (Kleisli IO) (P.Event G.Event) (P.Event Picture)

theMachine :: MainProc
theMachine = proc e ->
  do
    click0 <- P.filter $ arr ((Just Click ==) . filter0) -< e
    click5 <- P.filter $ arr ((Just Click ==) . filter5) -< e
    click10 <- P.filter $ arr ((Just Click ==) . filter10) -< e

    toggle0 <- P.filter $ arr ((Just Toggle ==) . filter0) -< e
    toggle5 <- P.filter $ arr ((Just Toggle ==) . filter5) -< e
    toggle10 <- P.filter $ arr ((Just Toggle ==) . filter10) -< e

    mode0 <- P.accum True -< not <$ toggle0
    mode5 <- P.accum True -< not <$ toggle5
    mode10 <- P.accum True -< not <$ toggle10

{-
    -- First order implementation

    count0 <- P.accum 0 <<< P.gather -< [(+1) <$ click0, const 0 <$ toggle0]
    count5 <- P.accum 0 -< (if mode5 then (+1) else id) <$ click5
    count10 <- P.accum 0 -< (+1) <$ click10

    let show0 = if mode0 then count0 else -1
    let show5 = if mode5 then count5 else -1
    let show10 = if mode10 then count10 else -1
-}

    -- Higher order implementation

    let newCounter0 = if mode0 then counter else arr $ const (-1)
    show0 <- P.drSwitch counter -< (click0, newCounter0 <$ toggle0)

    show5 <- 
        (let 
            next5 pa True = 
                P.dkSwitch pa (arr $ \((_, mode), _) -> mode) next5
            next5 pa False = 
                P.dSwitch (arr (const (-1)) *** id) (next5 pa)
          in
            next5 (arr fst >>> counter) True)
        -< (click5, mode5 <$ toggle5)

    let newTee = if mode10 then id else arr $ const (-1)
    count10 <- counter -< click10
    show10 <- P.drSwitch id -< (count10, newTee <$ toggle10)


    notice <- P.edge -< (show0, show5, show10)
    returnA -< renderButtons show0 show5 show10 <$ notice

  where
    counter = proc e -> P.accum 0 -< (+1) <$ e


feedEvent :: 
    G.Event -> 
    (MainProc, Picture) -> 
    IO (MainProc, Picture)
feedEvent e (pa, pic) =
  do
    (r, pa') <- runKleisli (P.stepRun pa) e
    let l = P.yields r
        pic' = if null l then pic else last l
    return (pa', pic')


main :: IO ()
main = 
    G.playIO
        (InWindow "Machinecell Example" (320, 240) (800, 200))
        white
        300
        (theMachine, renderButtons 0 0 0)
        (return . snd)
        feedEvent
        (\_ w -> return w)
