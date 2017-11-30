{-# LANGUAGE Arrows #-}
module Main where

-- gloss
import Graphics.Gloss

-- dunai
import Data.VectorSpace.Tuples ()

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

-- sonnendemo
import SonnenModel

main :: IO ()
main = flowGloss (InWindow "sonnen" (400, 640) (10, 10)) (greyN 0.9) 30 glossRhine


-- | The position of the coffee cup on the screen.
coffeePos = (300, 100)

-- | The size of the coffee cup rectangle that responds to mouse clicks.
coffeeSize = (15, 40)

-- | Determines whether a mouse click is inside the coffee cup.
onCoffee :: Point -> Bool
onCoffee pos
  let (x, y) = pos ^-^ coffeePos -- Relative position
  in  abs x < fst coffeeSize && abs y < snd coffeeSize


glossRhine :: GlossRhine ()
glossRhine = buildGlossSF select $ game
  where
    select (EventKey (MouseButton LeftButton) Down _ pos) | onCoffee pos = Just ()
    select _ = Nothing

game :: GlossSyncSF ()
game = gameLogic >>> graphics

graphics = _
