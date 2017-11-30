module SonnenModel where

-- | Modelling the current state of the coffee cup.
data CoffeeState
  = Empty
  | Brewing
  | Full
  | Drinking

brewingTime = 5
drinkingTime = 3


gameLogic = feedback 0 $ safely gameStates >>> batterySim

gameStates = do
  _ <- try $ proc (battery, event) -> do
    -- In case there is sufficient battery, brew a coffee when requested.
    _ <- throwOn () -< event && battery >= 1
    returnA         -< (Empty, 0)
  _ <- try $ arr (const Brewing) &&& scaledTimer brewingTime
  _ <- try $ arr snd >>> throwOn () >>> arr (const Full)
  _ <- try $ arr (const Drinking) &&& scaledTimer drinkingTime
  gameStates

batterySim = _
