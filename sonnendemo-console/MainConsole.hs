{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Main where

-- base
import Data.Char (toLower)

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Stdin
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.ResamplingBuffer.Collect
import FRP.Rhine.ResamplingBuffer.KeepLast
import FRP.Rhine.Schedule.Concurrently

-- sonnendemo
import SonnenModel


humanPrintWeather :: Weather -> String
humanPrintWeather Weather {..} = humanPrintSun sun ++ ", " ++ humanPrintWind wind
  where
    humanPrintSun  sun    = "It's " ++ (toLower <$> show sun)
    humanPrintWind Normal = "and the wind is normal."
    humanPrintWind Weak   = "but the wind is low! We need to pump energy in the grid!"
    humanPrintWind Strong = "but the wind is strong! We need to absorb energy from the grid!"

-- | The method to print a status message.
consoleOutput :: ModelState -> IO ()
consoleOutput ModelState {..} = do
  putStrLn $ humanPrintWeather weather
  putStrLn $ "The battery is charged at "
          ++ show (round $ batteryLevel * 100 / batteryCapacity)
          ++ "%."
  putStrLn $ case coffeeState of
    Empty      -> if batteryLevel < batteryBalancingMargin + coffeeEnergy
                    then "Your coffee is empty and the battery is low... :("
                    else "How about a nice brew? Just press Enter!"
    Brewing  _ -> "Brewing..."
    Full       -> "That's looking like a nice cuppa! Press Enter to enjoy!"
    Drinking _ -> "Mmmmmhhh..."

-- | The game loop.
game
  :: ClSF IO (RescaledClockFloat (Millisecond 40)) [a] ModelState
game = arr (not . null) >-> gameLogic

-- | The console output loop.
console :: ClSF IO (RescaledClockFloat (Millisecond 2000)) ModelState ()
console = arrMCl consoleOutput

mainRhine
  =   clId               @@  rescaleClockFloat StdinClock
  >-- collect            -@- concurrently
  --> game               @@  rescaleClockFloat waitClock
  >-- keepLast initModel -@- concurrently
  --> console            @@  rescaleClockFloat waitClock
  where
    initModel = ModelState
      { coffeeState  = Empty
      , batteryLevel = 0
      , weather      = Weather Sunny Normal
      , nCoffees     = 0
      }

main :: IO ()
main = flow mainRhine


-- * Helpers to deal with 'Float's instead of 'Double's as time differences.

-- | A newtype for 'UTCTime' with 'Float' instead of 'Double' as time difference.
newtype UTCTimeFloat = UTCTimeFloat UTCTime

instance TimeDomain UTCTimeFloat where
  type Diff (UTCTimeFloat) = Float
  diffTime (UTCTimeFloat t1) (UTCTimeFloat t2) = realToFrac $ diffTime t1 t2

-- | A clock rescaled to the 'TimeDomain' 'UTCTimeFloat'.
type RescaledClockFloat cl = RescaledClock cl UTCTimeFloat

-- | The rescaled clock value.
rescaleClockFloat :: Time cl ~ UTCTime => cl -> RescaledClockFloat cl
rescaleClockFloat cl = RescaledClock
  { rescale       = UTCTimeFloat
  , unscaledClock = cl
  }
