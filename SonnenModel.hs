{-# LANGUAGE Arrows       #-}
{-# LANGUAGE TypeFamilies #-}
module SonnenModel where


-- rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except


type Time        = Float
type Power       = Float
type Energy      = Float
type CoffeeLevel = Float

-- | Modelling the current state of the coffee cup.
data CoffeeState
  = Empty
  | Brewing CoffeeLevel
  | Full
  | Drinking CoffeeLevel
  deriving Show

isEmpty :: CoffeeState -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | The level to which the cup is filled with coffee.
coffeeLevel :: CoffeeState -> Float
coffeeLevel Empty        = 0
coffeeLevel (Brewing x)  = x
coffeeLevel Full         = 1
coffeeLevel (Drinking x) = 1 - x


brewingTime :: Time
brewingTime = 3

drinkingTime :: Time
drinkingTime = 2

batteryCapacity :: Energy
batteryCapacity = 3

-- | The energy needed to brew one cup of coffee.
coffeeEnergy :: Energy
coffeeEnergy = 0.9

-- | The maximum power that can be drained from or charged to the battery.
--   Coincidentally the same power needed to brew coffee.
batteryMaxPower :: Power
batteryMaxPower = coffeeEnergy / brewingTime

-- | The minimum charge that must remain in the battery
--   in order to supply primary control/balancing power.
batteryBalancingMargin :: Energy
batteryBalancingMargin = 0.5

-- TODO Use BehaviourF everywhere


gameLogic
  :: (Monad m, Clock m cl, Diff (TimeDomainOf cl) ~ Float)
  => SyncSF m cl Bool (CoffeeState, Energy, Weather)
-- BehaviourF m Time Bool (CoffeeState, Energy, Weather)
gameLogic = feedback 0 $ proc (coffeeRequest, batteryLevel) -> do
  weather       <- safely weatherStates -< ()
  coffeeState   <- safely gameStates    -< (coffeeRequest, batteryLevel)
  batteryLevel' <- batterySim           -< ( (solarPlant weather, coffeeState)
                                           , batteryLevel)
  returnA                               -< ( (coffeeState, batteryLevel', weather)
                                           , batteryLevel')

gameStates
  :: (Monad m, Clock m cl, Diff (TimeDomainOf cl) ~ Float)
  => SyncExcept m cl (Bool, Energy) CoffeeState Empty
gameStates = do
  _ <- try $ proc (coffeeRequest, batteryLevel) -> do
    -- In case there is sufficient battery, brew a coffee when requested.
    _ <- throwOn () -< coffeeRequest && batteryLevel >= coffeeEnergy + batteryBalancingMargin
    returnA         -< Empty
  _ <- try $ scaledTimer brewingTime >>> arr Brewing
  _ <- try $ arr fst >>> throwOn () >>> arr (const Full)
  _ <- try $ scaledTimer drinkingTime >>> arr Drinking
  gameStates

-- | Simulate the current battery charge level,
--   depending on the current solar power and the state of the coffee machine.
batterySim
  :: (Monad m, Diff td ~ Float)
  => BehaviourF m td ((Power, CoffeeState), Energy) Energy
batterySim = proc ((solarPower, coffeeState), batteryLevel) -> do
  let
    coffeeDrain = case coffeeState of
      Brewing _ -> batteryMaxPower
      _         -> 0
    solarInflux = if batteryLevel >= batteryCapacity - batteryBalancingMargin
      then 0
      else solarPower
    batteryTotalPower = solarInflux - coffeeDrain
  integral -< batteryTotalPower


-- | The possible states the weather can be in.
data Weather = Sunny | Cloudy | Night
  deriving Show


-- | Cycles through different weather states every 12 time steps.
--   Weather states were randomly selected by me ;)
weatherStates
  :: (Monad m, Clock m cl, Diff (TimeDomainOf cl) ~ Float)
  => SyncExcept m cl a Weather Empty
--BehaviourFExcept m Float a Weather Empty
weatherStates = do
  sequence_ [ try $ timer 12 >>> arr (const weather)
            | weather <- [Sunny, Night, Cloudy, Night]
            ]
  weatherStates

-- | Determines the power of the solar plant depending on the weather.
solarPlant :: Weather -> Float
solarPlant Sunny  = batteryMaxPower * 0.8
solarPlant Cloudy = batteryMaxPower / 4
solarPlant Night  = 0
