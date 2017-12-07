{- | This file contains the gloss backend to the game logic.
Game events (mouse clicks) are captured
-}

{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
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


-- * Some global variables for important elements on the screen

backgroundColor :: Color
backgroundColor = mixColors 10 20 green white

-- | The position of the coffee cup on the screen.
coffeePos :: Point
coffeePos = (160, 0)

coffeeSize :: Vector
coffeeSize = (50, 100)

-- | The size of the coffee cup rectangle that responds to mouse clicks.
coffeeCupSize :: Vector
coffeeCupSize = coffeeSize ^+^ (30, 30)

batteryPos :: Point
batteryPos = (0, 0)

batterySize :: Vector
batterySize = (30, 100)

solarPowerPos :: Point
solarPowerPos = (-150, 200)

windTurbineHeight :: Float
windTurbineHeight = 100

-- TODO Spread the coordinates to the graphics where they are needed

-- | Determines whether a mouse click is inside the coffee cup.
onCoffee :: Point -> Bool
onCoffee pos = abs x < fst coffeeCupSize && abs y < snd coffeeCupSize
  where
    (x, y) = pos ^+^ (0, snd coffeeSize) ^-^ coffeePos -- Relative position

-- * The different graphical elements

-- | Draw a cup of coffee, filled according to the given 'CoffeeState'.
coffeeCup :: CoffeeState -> Picture
coffeeCup coffeeState = pictures
  -- The cup
  [ translate (-40) 60 $ color white $ thickCircle 50 10
  , color white $ uncurry rectangleUpperSolid coffeeCupSize
  -- The coffee
  , color (dark $ dark $ dark $ dark yellow)
    $ translate 0 10
    $ rectangleUpperSolid (fst coffeeSize)
    $ snd coffeeSize * coffeeLevel coffeeState
  ]

-- | Draw a battery, filled to a given energy level.
battery :: Energy -> Picture
battery batteryLevel = pictures
  [ color (dark blue) $ uncurry rectangleUpperSolid $ batterySize ^+^ (30, 30)
  , color (greyN 80) $ translate 0 10 $ uncurry rectangleUpperSolid batterySize
  , color (light blue)
    $ translate 0 10
    $ rectangleUpperSolid (fst batterySize)
    $ snd batterySize * batteryLevel / batteryCapacity
  ]

wireThickness :: Float
wireThickness = 5

-- | Draw a wire of a given length
wire :: Float -> Picture
wire = color (greyN 0.05) . rectangleUpperSolid wireThickness

-- | Draw a little solar plant, with wires.
solarPower :: Picture
solarPower = pictures
  [ color (dark blue) $ polygon [ h ^+^ w, h ^-^ w, negate (h ^+^ w), w ^-^ h ]
  , rotate 180 $ wire down
  , translate 0 (wireThickness / 2 - down)
    $ rotate 270 $ wire
    $ fst solarPowerPos - fst batteryPos + fst batterySize
  ]
  where
    h    = (0 ,  10)
    w    = (50, -30)
    down = snd solarPowerPos - snd batteryPos - snd batterySize / 2

-- | A house containing the battery and the coffee machine.
house :: Picture
house = pictures $
  [ scale x 1 $ color (greyN 0.3) $ pictures
    [ translate 0 (d/2) $ rotate 90 $ rectangleUpperSolid d w
    , translate w 0 $ rectangleUpperSolid d h
    ]
  | x <- [1, -1]
  ]
  ++
  [ scale x 1 $ color (dark red) $ translate 0 (h + w)
    $ rotate (-135) $ translate 0 (-d/2) $ rectangleUpperSolid d 260
  | x <- [1, -1]
  ]
  where
    h = 140
    w = 160
    d = 10

-- | Draw a picture for the current state of the sun.
sunPicture :: Sun -> Picture
sunPicture Sunny  = color yellow $ pictures
  $ circleSolid 40 : [ rotate (45 * n) $ rectangleSolid 5 140 | n <- [0..3] ]
sunPicture Cloudy = translate (-100) 0 $ color (greyN 0.5) $ pictures
  [ translate (-40)  0 $ circleSolid 20
  , translate   35  20 $ circleSolid 10
  , translate (-15) 10 $ circleSolid 30
  , translate   15  10 $ circleSolid 30
  , translate   45   0 $ circleSolid 20
  ]
sunPicture Night  = translate 100 0 $ pictures
  [ color (light yellow) $ circleSolid 40
  , translate (-20) (-10) $ color backgroundColor $ circleSolid 40
  ]

-- | Horizontal position of the wind turbine
windTurbinePosX :: Float
windTurbinePosX = -300

-- | Draw a picture of the wind turbine where the rotors are at the given angle.
windTurbinePicture :: Float -> Picture
windTurbinePicture angle = pictures
  [ translate 0 10 $ rotate (-90) $ wire $ windTurbinePosX - fst batteryPos
  , color (greyN 0.95) $ rectangleUpperSolid 15 100
  , translate 0 100 $ rotate angle $ color white $ pictures $ circleSolid 15 :
      [ rotate (120 * n) $ rectangleUpperSolid 15 80
      | n <- [1,2,3]
      ]
  ]

-- | Determines how many degrees per second the wind turbine will turn.
windTurbineSpeed :: Wind -> Float
windTurbineSpeed Normal = 50
windTurbineSpeed Strong = 100
windTurbineSpeed Weak   = 5


-- | Combine all graphics into one picture.
graphics :: Monad m => BehaviourF m Float (CoffeeState, Energy, Weather) Picture
graphics = proc (coffeeState, batteryLevel, Weather {..}) -> do
  windTurbineAngle <- integral <<< average 0.3 -< windTurbineSpeed wind
  returnA                                      -< translate 0 (-200) $ pictures
    [ uncurry translate coffeePos $ pictures
      [ coffeeCup coffeeState
      -- Status symbol whether a coffee can be made
      , translate 0 (fst coffeeSize) $
          if batteryLevel < batteryBalancingMargin + coffeeEnergy && isEmpty coffeeState
            then color red $ pictures -- A cross (✘)
              [ rotate (40 * sign) $ rectangleSolid 10 100 | sign <- [1, -1] ]
            else translate 65 (-20) $ color green $ scale 0.8 0.8 $ pictures -- A check mark (✔)
              [ polygon [(-20,  20), (0, 0), (0, 15)]
              , polygon [( 40, 100), (0, 0), (0, 15)]
              ]
      ]
    , translate 0 400 $ sunPicture sun
    , uncurry translate solarPowerPos solarPower
    , translate windTurbinePosX 0 $ windTurbinePicture windTurbineAngle
    , uncurry translate batteryPos $ battery batteryLevel
    , translate 100 (-20) $ house
    ]


-- * The main program

-- | The main 'SyncSF' governing events, game logic and graphics.
--   An event is produced whenever the user has clicked on the cup at least once.
game :: GlossSyncSF ()
game = arr (not . null) >>> gameLogic >>> graphics


-- | The main 'Rhine' with event selection.
glossRhine :: GlossRhine ()
glossRhine = buildGlossRhine select game
  where
    select (EventKey (MouseButton LeftButton) Down _ pos) | onCoffee pos = Just ()
    select _ = Nothing

main :: IO ()
main = flowGloss (InWindow "sonnen" (800, 600) (10, 10)) backgroundColor 30 glossRhine
