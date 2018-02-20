module Data.Picture where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Global as Global
import Math as Math

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture -- difficult

instance showShape' :: Show Shape where
  show (Circle c r) =
    "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
  show (Rectangle c w h) =
    "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
  show (Line start end) =
    "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
  show (Text loc text) =
    "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
  show (Clipped p) = -- difficult
    "Clipped [" <> foldl (\acc s -> acc <> "\n  " <> s) "" (showShape <$> p) <> "\n]"

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
showShape (Clipped p) = -- difficult
  "Clipped [" <> foldl (\acc s -> acc <> "\n  " <> s) "" (showShape <$> p) <> "\n]"

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped p) = bounds p -- difficult

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

-- easy
circle1 :: Shape
circle1 = Circle origin 10.0
  where origin = Point { x: 0.0, y: 0.0 }

-- medium
double :: Shape -> Shape
double shape = case shape of
  Circle _ r -> Circle center (r * 2.0)
  Rectangle _ w h -> Rectangle center (w * 2.0) (h * 2.0)
  Text _ t -> Text center t
  other -> other
  where center = Point { x: 0.0, y: 0.0 }

-- medium
extractText :: Shape -> Maybe String
extractText (Text _ t) = Just t
extractText _ = Nothing

-- medium
area :: Shape -> Number
area (Circle _ r) = Math.pi * r * r
area (Rectangle _ w h) = w * h
area (Clipped p) = let
  Bounds { top, left, bottom, right } = bounds p
  w = Math.abs (right - left)
  h = Math.abs (bottom - top)
  in w * h
area _ = 0.0