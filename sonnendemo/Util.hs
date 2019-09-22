{- | Several utilities for MainGloss-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Util where

-- gloss
import Graphics.Gloss.Interface.Pure.Game


-- dunai
import Data.VectorSpace

-- | Determines whether a given point is in a given rectangle.
inRectangle
  :: Point -- ^ The point to test
  -> Point -- ^ The centre of the rectangle
  -> Vector -- ^ The size of the rectangle as @(width, height)@
  -> Bool
inRectangle point centre (width, height) = let (x, y) = point ^-^ centre
  in abs x < width / 2 && abs y < height /2

-- | Determines whether a given point is in a given upper rectangle.
inUpperRectangle
  :: Point -- ^ The point to test
  -> Point -- ^ The base, or bottom, of the rectangle
  -> Vector -- ^ The size of the rectangle as @(width, height)@
  -> Bool
inUpperRectangle point base size
  = inRectangle point (base ^+^ (0, snd size / 2)) size

deriving via FractionalVectorSpace Float instance InnerProductSpace Float
deriving via FractionalVectorSpace Float instance NormedSpace Float
