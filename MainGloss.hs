{- | This file contains the gloss backend to the game logic.
Game events (mouse clicks) are captured
-}

{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

-- gloss
import Graphics.Gloss.Interface.Pure.Game -- TODO This should be reexported by rhine-gloss

-- dunai
import Data.VectorSpace

-- rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except

-- rhine-gloss
import FRP.Rhine.Gloss

-- sonnendemo
import SonnenModel
import Util


-- * Important graphical elements

-- | The background colour.
backgroundColor :: Color
backgroundColor = mixColors 10 20 green white

-- | The amount by which the whole picture is offset vertically.
yOffset :: Float
yOffset = -200

-- ** In the house

-- *** The coffee cup

-- | The position of the coffee cup (bottom) on the screen.
coffeePos :: Point
coffeePos = (160, 0)

-- | The size of the coffee cup, in the format @(width, height)@.
coffeeSize :: Vector
coffeeSize = (50, 100)

-- | The size of the coffee cup rectangle that responds to mouse clicks.
coffeeCupSize :: Vector
coffeeCupSize = coffeeSize ^+^ (30, 30)

-- | Determines whether a mouse click is inside the coffee cup.
onCoffee :: Point -> Bool
onCoffee pos = inUpperRectangle pos (coffeePos ^+^ (0, yOffset)) coffeeCupSize

-- | Draw a cup of coffee, filled according to the given 'CoffeeState'.
coffeeCup :: CoffeeState -> Picture
coffeeCup coffeeState = contoured 2 $ pictures
  -- The cup
  [ translate (-40) 60 $ color white $ thickCircle 50 10
  , color white $ uncurry rectangleUpperSolid coffeeCupSize
  -- The coffee
  , color (dark $ dark $ dark $ dark orange)
    $ translate 0 10
    $ rectangleUpperSolid (fst coffeeSize)
    $ snd coffeeSize * coffeeLevel coffeeState
  ]

-- *** The battery

batteryPos :: Point
batteryPos = (0, 0)

batterySize :: Vector
batterySize = (30, 100)

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

-- *** The house

-- | A house containing the battery and the coffee machine.
house :: Picture
house = contoured 2 $ pictures $
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


-- ** The power plants

-- *** The solar plant

solarPowerPos :: Point
solarPowerPos = (-150, 200)


-- | Draw a little solar plant, with wires.
solarPower :: Picture
solarPower = pictures
  [ rotate 180 $ wire down
  , translate 0 (wireThickness / 2 - down)
    $ rotate 270 $ wire
    $ fst solarPowerPos - fst batteryPos + fst batterySize
  , color (dark blue) $ polygon [ h ^+^ w, h ^-^ w, negate (h ^+^ w), w ^-^ h ]
  ]
  where
    h    = (0 ,  10)
    w    = (50, -30)
    down = snd solarPowerPos - snd batteryPos - snd batterySize / 2


-- *** The wind turbine

windTurbineHeight :: Float
windTurbineHeight = 100

-- | Horizontal position of the wind turbine
windTurbinePosX :: Float
windTurbinePosX = -300

-- | Draw a picture of the wind turbine where the rotors are at the given angle.
windTurbinePicture :: Float -> Picture
windTurbinePicture angle = pictures
  [ translate 0 10 $ rotate (-90) $ wire $ windTurbinePosX - fst batteryPos
  , contoured 2 $ color (greyN 0.95) $ rectangleUpperSolid 15 100
  , contoured 2 $ translate 0 100 $ rotate angle $ color white $ pictures
      $ circleSolid 15 :
      [ rotate (120 * n) $ rectangleUpperSolid 15 80
      | n <- [1,2,3]
      ]
  ]

-- | Determines how many degrees per second the wind turbine will turn.
windTurbineSpeed :: Wind -> Float
windTurbineSpeed Normal = 50
windTurbineSpeed Strong = 100
windTurbineSpeed Weak   = 5

-- *** The wiring

wireThickness :: Float
wireThickness = 5

-- | Draw a wire of a given length
wire :: Float -> Picture
wire = color (greyN 0.05) . rectangleUpperSolid wireThickness


-- *** The weather

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

-- ** Animated descriptions

dancingArrow :: Monad m => Behaviour m Float Picture
dancingArrow = proc _ -> do
  time <- timeInfoOf absolute -< ()
  returnA                     -< contoured 2 $ color (greyN 0.7)
    $ translate 0 (sin (time * 5) * 30) $ pictures
      [ rotate (-135) $ translate 0 (-d/2) $ rectangleUpperSolid d h
      , rotate   135  $ translate 0 (-d/2) $ rectangleUpperSolid d h
      , rotate   180  $ rectangleUpperSolid d l
      ]
  where
    d = 20
    h = 50
    l = 60

description :: Monad m => BehaviourF m Float String Picture
description = proc string -> do
  arrow <- dancingArrow -< ()
  returnA               -< pictures
    [ arrow
    , translate 50 0 $ scale 0.2 0.2 $ pictures
      [ translate 0 ((-120) * i) $ text line
      | (i, line) <- zip [1..] $ lines string
      ]
    ]

-- | @slowly as dt@ returns the first @n@ elements
--   of the list @as@ after the time @n * dt@.
slowly :: Monad m => [a] -> Float -> Behaviour m Float [a]
slowly as dt = proc _ -> do
  time <- integral  -< 1 / dt
  returnA           -< take (round time) as

placeBelow :: Monad m => Point -> BehaviourF m Float Picture Picture
placeBelow point = arr $ uncurry translate $ point ^-^ (0, 40)

placeMsg :: Monad m => Point -> String -> BehaviourF m Float () Picture
placeMsg point msg = slowly msg 0.06 >>> description >>> placeBelow point

-- | Create a step in the tutorial descriptions.
tutorialDescriptionStep
  :: Monad m
  => String -- ^ The message to display
  -> Point  -- ^ Where to display it
  -> (ModelState -> Bool) -- ^ The condition after which to move to the next step
  -> BehaviourFExcept m Float ModelState Picture ()
tutorialDescriptionStep msg point condition = try
  $   throwOnCond condition () >>> arr (const ())
  >>> placeMsg point msg


-- | When playing for the first times, give helpful text descriptions.
tutorialDescriptions :: Monad m => BehaviorF m Float ModelState Picture
tutorialDescriptions = safely $ do
  tutorialDescriptionStep
    "Welcome to sonnendemo!\nWait until the battery is charged..."
    batteryPos
    $ \ModelState {..} -> batteryLevel >= coffeeEnergy + batteryBalancingMargin
  tutorialDescriptionStep
    "Click on the cup\nto make a coffee!"
    coffeePos
    $ \ModelState {..} -> coffeeState == Full
  tutorialDescriptionStep
    "Click again to\ndrink the coffee!"
    coffeePos
    $ \ModelState {..} -> isDrinking coffeeState
  tutorialDescriptionStep
    "Mmmmhhhh..."
    coffeePos
    $ \ModelState {..} -> isEmpty coffeeState
  wind <- try $ proc ModelState { weather = Weather {..} } -> do
    time <- timeSinceSimStart -< ()
    _    <- throwOn'          -< ( wind `elem` [Strong, Weak] && time > 4
                                 , wind)
    placeMsg (windTurbinePosX, 0)
      $  "But there is much more to explore!\n"
      ++ "Watch how the wind speed changes..." -< ()
  let fineUntil nextWind = tutorialDescriptionStep
        "Phew! We're fine again.\nBut what else could happen..?"
        (windTurbinePosX, 0)
        $ \ModelState { weather = Weather {..} } -> wind == nextWind
      strongWind = tutorialDescriptionStep
        "Too much electrity is generated!\nThe surplus has to be stored in the battery!"
        (windTurbinePosX, 0)
        $ \ModelState { weather = Weather {..} } -> wind == Normal
      weakWind = tutorialDescriptionStep
        "Not enough electrity is generated!\nThe grid needs energy from the battery!"
        (windTurbinePosX, 0)
        $ \ModelState { weather = Weather {..} } -> wind == Normal
  case wind of
    Strong -> do
      strongWind
      fineUntil Weak
      weakWind
    Weak -> do
      weakWind
      fineUntil Strong
      strongWind
  tutorialDescriptionStep
    "That's it!\nHave fun and enjoy your coffees!\nPress Escape to quit."
    coffeePos
    $ \ModelState {..} -> isBrewing coffeeState
  safe $ arr $ const $ Blank


-- ** Combining everything

-- | Combine all graphics into one picture.
graphics :: Monad m => BehaviourF m Float ModelState Picture
graphics = proc model@ModelState { weather = Weather {..}, .. } -> do
  windTurbineAngle <- integral <<< average 0.3 -< windTurbineSpeed wind
  description      <- tutorialDescriptions     -< model
  returnA                                      -< translate 0 yOffset $ pictures
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
    , translate 200 440
      $ scale 0.2 0.2 $ text $ "Coffees: " ++ show nCoffees
    , description
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

-- * Available in rhine-0.5.0.0

timeSinceSimStart :: (Monad m, TimeDomain td) => Behaviour m td (Diff td)
timeSinceSimStart = proc _ -> do
  time         <- timeInfoOf absolute -< ()
  simStartTime <- keepFirst           -< time
  returnA                             -< time `diffTime` simStartTime


-- | Like 'timer_', but doesn't output the remaining time at all.
timer_
  :: ( Monad m
     , TimeDomain td
     , Ord (Diff td)
     )
  => Diff td
  -> BehaviorF (ExceptT () m) td a ()
timer_ diff = timer diff >>> arr (const ())
-- * To be ported to rhine

throwOnCond cond e = proc a -> if cond a
  then throwS  -< e
  else returnA -< a
