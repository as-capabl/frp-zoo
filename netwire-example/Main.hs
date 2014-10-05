{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module
    Main
where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Category
import Control.Arrow

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import Control.Wire

import Buttons


-- Definition
period :: Double
period = 1.0


-- Utility
accumHold :: 
    (Monoid e, Monad m) => 
    a -> Wire s e m (Event (a->a)) a
accumHold x = (hold <|> pure x) . accumE (flip ($)) x


-- Main body
theWire ::
    Wire (Timed Double ()) () IO (G.Event) Picture
theWire = proc eSrc ->
  do
    -- `eSrc` will be updated at t = 0.0, 1.0, 2.0, ...
    -- Event `e` will occur at t = 0.5, 1.5, ...
    e <- for (period/2) . never --> periodic period -< eSrc


    click0 <- filterE ((Just Click ==) . filter0) -< e
    click5 <- filterE ((Just Click ==) . filter5) -< e
    click10 <- filterE ((Just Click ==) . filter10) -< e

    toggle0 <- filterE ((Just Toggle ==) . filter0) -< e
    toggle5 <- filterE ((Just Toggle ==) . filter5) -< e
    toggle10 <- filterE ((Just Toggle ==) . filter10) -< e

    mode0 <- accumHold True -< not <$ toggle0
    mode5 <- accumHold True -< not <$ toggle5
    mode10 <- accumHold True -< not <$ toggle10

    -- Every toggle event causes switch of counters, with every counter is newly created.
    -- This is NOT EXACTLY the required implementation.
    let newCounter0 = if mode0 then counter else arr $ const (-1)
    count0 <- rSwitch counter -< (click0, newCounter0 <$ toggle0)

    -- `modes` can switch multiple state with inactive one suspended.
    -- I don't know why `modes` is special for netwire. 
    count5 <- 
        modes True (\case {True -> counter; False -> pure (-1)})
            -< (click5, mode5 <$ toggle5)

    -- <|> operator is suitable for scenario 10.
    count10 <- 
        pure (-1) . unless snd  <|> counter . arr fst
            -< (click10, mode10)

    returnA -< renderButtons count0 count5 count10
  where
    counter = proc e -> accumHold 0 -< (+1) <$ e

main :: IO ()
main =
  do
    G.playIO
        (InWindow "Netwire Example" (320, 240) (800, 200))
        white
        300
        (theWire, countSession_ period, renderButtons 0 0 0)
        (\(_, _, pic) -> return pic)
        (\e (w, session, pic) ->
          do
            (s, session') <- stepSession session
            (r, w') <- stepWire w s (Right e)
            return (w', session', either (const pic) id r))
        (\_ w -> return w)
