{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Main where

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Stdin
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.ResamplingBuffer.Collect
import FRP.Rhine.ResamplingBuffer.KeepLast
import FRP.Rhine.Schedule.Concurrently

-- sonnendemo
import SonnenModel


-- | The method to print a status message.
consoleOutput :: (CoffeeState, Energy, Weather) -> IO ()
consoleOutput (coffeeState, batteryLevel, weather) = do
  putStrLn $ "The weather is " ++ show weather ++ "."
  putStrLn $ "The battery is charged at "
          ++ show (batteryLevel * 100 / batteryCapacity)
          ++ " %."
  putStrLn $ case coffeeState of
    Empty      -> if batteryLevel < batteryBalancingMargin + coffeeEnergy
                    then "Your coffee is empty and the battery is low... :("
                    else "How about a nice brew? Just press Enter!"
    Brewing  _ -> "Brewing..."
    Full       -> "That's looking like a nice cuppa! Press Enter to enjoy!"
    Drinking _ -> "Mmmmmhhh..."

-- | The game loop.
game
  :: SyncSF IO (RescaledClockFloat (Millisecond 40))
       [a] (CoffeeState, Energy, Weather)
game = arr (not . null) >-> gameLogic

-- | The console output loop.
console
  :: SyncSF IO (RescaledClockFloat (Millisecond 2000))
       (CoffeeState, Energy, Weather) ()
console = arrMSync consoleOutput

mainRhine
  =   syncId                                    @@  rescaleClockFloat StdinClock
  >-- collect                                   -@- concurrently
  --> game                                      @@  rescaleClockFloat waitClock
  >-- keepLast (Empty, 0, Weather Sunny Normal) -@- concurrently
  --> console                                   @@  rescaleClockFloat waitClock

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
rescaleClockFloat :: TimeDomainOf cl ~ UTCTime => cl -> RescaledClockFloat cl
rescaleClockFloat cl = RescaledClock
  { rescale = UTCTimeFloat
  , unscaledClock = cl
  }
