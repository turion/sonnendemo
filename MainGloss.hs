{- | This file contains the gloss backend to the game logic.
Game events (mouse clicks) are captured
-}

{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
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

-- * 'VectorSpace' utilities (might be in a future dunai release)

-- TODO This instance might be in a future dunai release,
-- see https://github.com/ivanperez-keera/dunai/pull/67
instance NormedSpace (Float, Float) where
  norm (x, y) = sqrt $ x ^ 2 + y ^ 2
normalize v = v ^* (1 / norm v)

-- * Gloss utilities

contourPath :: Float -> Path -> Path
contourPath d left@(p1 : p2 : p3 : ps) = map f $ zip3 left middle right
  where
    middle = p2 : p3 : ps ++ [p1]
    right  = p3 : ps ++ [p1, p2]
    rotate90 (x, y) = (-y, x)
    f (l, m, r) =
      let
        v = normalize $ l ^-^ m
        w = normalize $ r ^-^ m
        dl = (-d) *^ (rotate90 v)
        dr = d *^ (rotate90 w)
      in m ^+^ dr ^-^ (w ^* (norm (dl ^-^ dr) / norm (v ^-^ w)))
contourPath _ _ = error "Path must at least contain three points."

-- | Fills a slightly larger version of the given picture with a darker colour.
--   Can be used to give a contour or shadow to the original picture.
--   Only operates on thick pictures, e.g. `ThickCircle`, `Polygon` etc.
contourFill
  :: Float -- ^ How thick the contour should be
  -> Picture -- ^ The initial picture
  -> Picture -- ^ The contour fill (to be drawn underneath the picture)
contourFill _ Blank = Blank
contourFill d (Polygon path)
  | length path >= 3 = Polygon $ contourPath d path
  | otherwise        = Blank -- Improper polygons are disregarded
contourFill _ (Line _) = Blank
contourFill _ (Circle _) = Blank
contourFill d (ThickCircle radius thickness) = ThickCircle radius $ thickness + d * 2
contourFill _ (Arc _ _ _) = Blank
contourFill d (ThickArc angle1 angle2 radius thickness)
  = ThickArc (angle1 - dAngle) (angle2 + dAngle) radius $ thickness + d * 2
  where
    dAngle = 180 * d / (pi * radius)
contourFill _ (Text _) = Blank
contourFill _ (Bitmap _ _ _ _) = Blank
contourFill d (Color color picture) = Color (dark color) $ contourFill d picture
contourFill d (Translate x y picture) = Translate x y $ contourFill d picture
contourFill d (Rotate angle picture) = Rotate angle $ contourFill d picture
contourFill d (Scale x y picture) = Scale x y $ contourFill d picture
contourFill d (Pictures pictures) = Pictures $ contourFill d <$> pictures

contoured :: Float -> Picture -> Picture
contoured d picture = pictures [ contourFill d picture, picture ]

-- * Some global variables for important elements on the screen

backgroundColor :: Color
backgroundColor = mixColors 10 20 green white

-- | The position of the coffee cup on the screen.
coffeePos :: Point
coffeePos = (100, 0)

coffeeSize :: Vector
coffeeSize = (50, 100)

-- | The size of the coffee cup rectangle that responds to mouse clicks.
coffeeCupSize :: Vector
coffeeCupSize = coffeeSize ^+^ (30, 30)

batteryPos :: Point
batteryPos = (-100, 0)

batterySize :: Vector
batterySize = (30, 100)

solarPowerPos :: Point
solarPowerPos = (-250, 200)

-- | Determines whether a mouse click is inside the coffee cup.
onCoffee :: Point -> Bool
onCoffee pos = abs x < fst coffeeCupSize && abs y < snd coffeeCupSize
  where
    (x, y) = pos ^+^ (0, snd coffeeSize) ^-^ coffeePos -- Relative position

-- * The different graphical elements

-- | Draw a cup of coffee, filled according to the given 'CoffeeState'.
coffeeCup :: CoffeeState -> Picture
coffeeCup coffeeState = contoured 2 $ pictures
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
battery batteryLevel = contoured 2 $ pictures
  [ color (dark blue) $ uncurry rectangleUpperSolid $ batterySize ^+^ (30, 30)
  , color (greyN 80) $ translate 0 10 $ uncurry rectangleUpperSolid batterySize
  , color (light blue)
    $ translate 0 10
    $ rectangleUpperSolid (fst batterySize)
    $ snd batterySize * batteryLevel / batteryCapacity
  ]

-- | Draw a little solar plant, with wires.
solarPower :: Picture
solarPower = contoured 2 $ color (dark blue) $ pictures
  [ polygon [ h ^+^ w, h ^-^ w, negate (h ^+^ w), w ^-^ h ]
  , rotate 180 $ rectangleUpperSolid wire down
  , translate 0 (wire / 2 - down)
    $ rotate 270 $ rectangleUpperSolid wire
    $ fst solarPowerPos - fst batteryPos + fst batterySize
  ]
  where
    h    = (0 ,  10)
    w    = (50, -30)
    wire = 5
    down = snd solarPowerPos - snd batteryPos - snd batterySize / 2


-- | Draw a picture for the current weather.
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

-- | Combine all graphics into one picture.
graphics :: (CoffeeState, Energy, Weather) -> Picture
graphics (coffeeState, batteryLevel, weather) = translate 0 (-200) $ pictures
  [ uncurry translate coffeePos $ pictures
    [ coffeeCup coffeeState
    -- Status symbol whether a coffee can be made
    , translate 0 (fst coffeeSize) $
        if batteryLevel < batteryBalancingMargin + coffeeEnergy && isEmpty coffeeState
          then color red $ pictures -- A cross (✘)
            [ rotate (40 * sign) $ rectangleSolid 10 100 | sign <- [1, -1] ]
          else translate 70 0 $ color green $ pictures -- A check mark (✔)
            [ polygon [(-20,  20), (0, 0), (0, 15)]
            , polygon [( 40, 100), (0, 0), (0, 15)]
            ]
    ]
  , uncurry translate batteryPos $ battery batteryLevel
  , translate 0 300 $ weatherPicture weather
  , uncurry translate solarPowerPos solarPower
  ]


-- * The main program

-- | The main 'SyncSF' governing events, game logic and graphics.
--   An event is produced whenever the user has clicked on the cup at least once.
game :: GlossSyncSF ()
game = arr (not . null) >>> gameLogic >>> arr graphics


-- | The main 'Rhine' with event selection.
glossRhine :: GlossRhine ()
glossRhine = buildGlossRhine select game
  where
    select (EventKey (MouseButton LeftButton) Down _ pos) | onCoffee pos = Just ()
    select _ = Nothing

main :: IO ()
main = flowGloss (InWindow "sonnen" (640, 480) (10, 10)) backgroundColor 30 glossRhine
