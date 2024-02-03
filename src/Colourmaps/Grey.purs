module Colourmaps.Grey (colourmap) where

colourmap :: Number -> { r :: Number, g :: Number, b :: Number, a :: Number }
colourmap n =
  { r: n, g: n, b: n, a: 1.0}
