{-# LANGUAGE Arrows #-}
module Main where

-- gloss
import Graphics.Gloss.Interface.Pure.Game -- TODO This should be reexported by rhine-gloss

-- dunai
import Data.VectorSpace
import Data.VectorSpace.Tuples ()

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss

-- sonnendemo
import SonnenModel

main :: IO ()
main = flowGloss (InWindow "sonnen" (640, 480) (10, 10)) backgroundColor 30 glossRhine


backgroundColor = mixColors 10 20 green white

-- | The position of the coffee cup on the screen.
coffeePos = (100, 0)

coffeeSize = (50, 100)

-- | The size of the coffee cup rectangle that responds to mouse clicks.
coffeeCupSize = coffeeSize ^+^ (30, 30)

batteryPos = (-100, 0)
batterySize = (30, 100)

solarPowerPos = (-250, 200)

-- | Determines whether a mouse click is inside the coffee cup.
onCoffee :: Point -> Bool
onCoffee pos = abs x < fst coffeeCupSize && abs y < snd coffeeCupSize
  where
    (x, y) = pos ^+^ (0, snd coffeeSize) ^-^ coffeePos -- Relative position


glossRhine :: GlossRhine ()
glossRhine = buildGlossRhine select game
  where
    select (EventKey (MouseButton LeftButton) Down _ pos) | onCoffee pos = Just ()
    select _ = Nothing

game :: GlossSyncSF ()
game = arr (not . null) >>> gameLogic >>> arr graphics

graphics :: (CoffeeState, Energy, Weather) -> Picture
graphics (coffeeState, batteryLevel, weather) = translate 0 (-200) $ pictures
  [ uncurry translate coffeePos $ pictures
    -- The cup
    [ translate (-40) 60 $ pictures
      [color white $ circleSolid 50, color backgroundColor $ circleSolid 30]
    , color white $ uncurry rectangleUpperSolid coffeeCupSize
    -- The coffee
    , color (dark $ dark $ dark $ dark yellow)
      $ translate 0 10
      $ rectangleUpperSolid (fst coffeeSize)
      $ snd coffeeSize * coffeeLevel coffeeState
    -- Status symbol whether a coffee can be made
    , translate 0 (fst coffeeSize) $
        if batteryLevel < batteryBalancingMargin + coffeeEnergy && isEmpty coffeeState
          then color red $ pictures
            [ rotate (40 * sign) $ rectangleSolid 10 100 | sign <- [1, -1] ]
          else translate 70 0 $ color green $ pictures
            [ polygon [(-20,  20), (0, 0), (0, 15)]
            , polygon [( 40, 100), (0, 0), (0, 15)]
            ]
    ]
  -- The battery
  , uncurry translate batteryPos $ pictures
      [ color (dark blue) $ uncurry rectangleUpperSolid $ batterySize ^+^ (30, 30)
      , color (dark blue) $ uncurry translate (batterySize ^/ 2)
        $ rotate 90 $ rectangleUpperSolid wire
        $ fst solarPowerPos - fst batteryPos - fst batterySize / 2 - wire / 2
      , color (greyN 80) $ translate 0 10 $ uncurry rectangleUpperSolid batterySize
      , color (light blue)
        $ translate 0 10
        $ rectangleUpperSolid (fst batterySize)
        $ snd batterySize * batteryLevel / batteryCapacity
      ]
  -- The weather
  , translate 0 300 $ weatherPicture weather
  -- The solar plant
  , uncurry translate solarPowerPos $ color (dark blue) $ pictures
    [ polygon [ h ^+^ w, h ^-^ w, negate (h ^+^ w), w ^-^ h ]
    , rotate 180 $ rectangleUpperSolid wire
      $ snd solarPowerPos - snd batteryPos - snd batterySize / 2
    ]
  ]
  where
    h    = (0 ,  10)
    w    = (50, -30)
    wire = 5

weatherPicture :: Weather -> Picture
weatherPicture Sunny  = translate (-100) 0 $ color yellow $ pictures
  $ circleSolid 40 : [ rotate (45 * n) $ rectangleSolid 5 140 | n <- [0..3] ]
weatherPicture Cloudy = color (greyN 0.5) $ pictures
  [ translate (-40)  0 $ circleSolid 20
  , translate   35  20 $ circleSolid 10
  , translate (-15) 10 $ circleSolid 30
  , translate   15  10 $ circleSolid 30
  , translate   45   0 $ circleSolid 20
  ]
weatherPicture Night  = translate 100 0 $ pictures
  [ color (light yellow) $ circleSolid 40
  , translate (-20) (-10) $ color backgroundColor $ circleSolid 40
  ]
