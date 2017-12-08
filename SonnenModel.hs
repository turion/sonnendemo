{- | The backend-agnostic model for simulating the virtual power plant.
To verify that the model can be simulated with different clocks,
the type 'BehaviourF' is used, which is clock-polymorphic.
-}

{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module SonnenModel where


-- rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except

-- * General type synonyms

type Time        = Float
type Power       = Float
type Energy      = Float
type CoffeeLevel = Float

-- * Components of the simulation

-- ** Coffee cup and machine

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
brewingTime = 2

drinkingTime :: Time
drinkingTime = 2

-- | The energy needed to brew one cup of coffee.
coffeeEnergy :: Energy
coffeeEnergy = brewingTime * batteryMaxPower


-- | The different states of the coffee machine,
--   depending on user coffee requests and battery charge level.
coffeeStates
  :: (Monad m, TimeDomain td, Diff td ~ Float)
  => BehaviourFExcept m td  (Bool, Energy) CoffeeState Empty
coffeeStates = do
  -- In case there is sufficient battery, start brewing a coffee when requested
  try $ proc (coffeeRequest, batteryLevel) -> do
    _ <- throwOn () -< coffeeRequest
                    && batteryLevel >= coffeeEnergy + batteryBalancingMargin
    returnA         -< Empty
  -- Output the progress of the brewing process until the time is up
  try $ scaledTimer brewingTime >>> arr Brewing
  -- Start drinking the coffee when requested
  try $ proc (drinkRequest, _) -> do
    _ <- throwOn () -< drinkRequest
    returnA         -< Full
  -- Output the progress of the drinking process until the time is up
  try $ scaledTimer drinkingTime >>> arr Drinking
  coffeeStates

-- ** The battery

batteryCapacity :: Energy
batteryCapacity = 4

-- | The maximum power that can be drained from or charged to the battery.
batteryMaxPower :: Power
batteryMaxPower = 0.3

-- | The minimum time under which the battery is required to be able to
--   supply primary control/balancing power.
batteryBalancingTime :: Time
batteryBalancingTime = 4

-- | The minimum charge that must remain in the battery
--   in order to supply primary control/balancing power.
batteryBalancingMargin :: Energy
batteryBalancingMargin = batteryMaxPower * batteryBalancingTime


-- | Simulate the current battery charge level,
--   depending on the current solar power and the state of the coffee machine.
batterySim
  :: (Monad m, TimeDomain td, Diff td ~ Float)
  => BehaviourF m td ((Weather, CoffeeState), Energy) Energy
batterySim = proc ((Weather {..}, coffeeState), batteryLevel) -> do
  let
    coffeeDrain = case coffeeState of
      Brewing _ -> batteryMaxPower
      _         -> 0
    solarInflux = if batteryLevel >= batteryCapacity - batteryBalancingMargin
      then 0
      else solarPlant sun
    windInflux = if batteryLevel >= batteryCapacity - batteryBalancingMargin
                 && wind /= Strong
      then -batteryMaxPower
      else windTurbine wind
    batteryTotalPower = solarInflux + windInflux - coffeeDrain
  integral -< batteryTotalPower


-- ** The weather

-- | The complete current state of the weather
data Weather = Weather
  { sun  :: Sun
  , wind :: Wind
  }
  deriving Show

-- | Simulates the weather changing over time
theWeather
  :: (Monad m, TimeDomain td, Diff td ~ Float)
  => Behaviour m td Weather
theWeather = proc _ -> do
  sun  <- safely sunStates  -< ()
  wind <- safely windStates -< ()
  returnA                   -< Weather {..}


-- | The possible states the sun can be in.
data Sun = Sunny | Cloudy | Night
  deriving Show

-- | Cycles through different sun states every 12 time steps.
--   The states were randomly selected by me ;)
sunStates
  :: (Monad m, TimeDomain td, Diff td ~ Float)
  => BehaviourFExcept m td a Sun Empty
--BehaviourFExcept m Float a Weather Empty
sunStates = do
  sequence_ [ try $ timer 12 >>> arr (const weather)
            | weather <- [Sunny, Night, Cloudy, Night]
            ]
  sunStates


-- | The possible wind strengths
data Wind = Normal | Strong | Weak
  deriving (Show, Eq)


windStates
  :: (Monad m, TimeDomain td, Diff td ~ Float)
  => BehaviourFExcept m td a Wind Empty
windStates = do
  try $ timer 7.5                  >>> arr (const Normal)
  try $ timer batteryBalancingTime >>> arr (const Strong)
  try $ timer 9.5                  >>> arr (const Normal)
  try $ timer batteryBalancingTime >>> arr (const Weak)
  windStates

-- ** The power plants

-- | Determines the power of the solar plant depending on the sun.
solarPlant :: Sun -> Float
solarPlant Sunny  = batteryMaxPower * 0.5
solarPlant Cloudy = batteryMaxPower * 0.2
solarPlant Night  = 0

-- | Determines the power (or lack thereof)
--   of the wind turbine depending on the weather.
windTurbine :: Wind -> Float
windTurbine Normal = batteryMaxPower * 0.2
windTurbine Strong = batteryMaxPower * 0.5
windTurbine Weak   = batteryMaxPower * (-0.5)

-- * Putting everything together

-- | The logic of the whole model.
--   Requests to make coffees can be input
--   (where 'True' represents a request, and 'False' no request),
--   and the current state of the coffee machine and cup,
--   the battery charge level and the current weather are output.
gameLogic
  :: (Monad m, TimeDomain td, Diff td ~ Float)
  => BehaviorF m td Bool (CoffeeState, Energy, Weather)
gameLogic = feedback 0 $ proc (coffeeRequest, batteryLevel) -> do
  weather       <- theWeather          -< ()
  coffeeState   <- safely coffeeStates -< (coffeeRequest, batteryLevel)
  batteryLevel' <- batterySim          -< ( (weather, coffeeState)
                                          , batteryLevel)
  returnA                              -< ( (coffeeState, batteryLevel', weather)
                                          , batteryLevel')
