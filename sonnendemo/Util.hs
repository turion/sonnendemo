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

-- * Gloss utilities

-- | Calculate a path that is a given width thicker than the original path.
contourPath :: Float -> Path -> Path
contourPath d left@(p1 : p2 : p3 : ps) = map f $ zip3 left middle right
  where
    middle = p2 : p3 : ps ++ [p1]
    right  = p3 : ps ++ [p1, p2]
    rotate90 (x, y) = (-y, x)
    f (l, m, r) =
      let
        mynormalize vec = if norm vec == 0 then zeroVector else normalize vec
        v = mynormalize $ l ^-^ m
        w = mynormalize $ r ^-^ m
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
contourFill _ (Bitmap _) = Blank
contourFill d (Color color picture) = Color (dark color) $ contourFill d picture
contourFill d (Translate x y picture) = Translate x y $ contourFill d picture
contourFill d (Rotate angle picture) = Rotate angle $ contourFill d picture
contourFill d (Scale x y picture) = Scale x y $ contourFill d picture
contourFill d (Pictures pictures) = Pictures $ contourFill d <$> pictures

-- | Create a version of the picture with a contour.
contoured :: Float -> Picture -> Picture
contoured d picture = pictures [ contourFill d picture, picture ]
