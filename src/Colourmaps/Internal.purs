module Colourmaps.Internal where

import Prelude

import Data.Array (unsafeIndex)
import Data.Int as Int
import Data.Number as Number
import Partial.Unsafe (unsafePartial)

colourMapDataToFunction :: Array (Array Number) -> Int -> { r :: Number, g :: Number, b :: Number, a :: Number }
colourMapDataToFunction dataArray n =
  let
    _data =
      unsafePartial $ unsafeIndex dataArray n

    red =
      unsafePartial $ unsafeIndex _data 0

    green =
      unsafePartial $ unsafeIndex _data 1

    blue =
      unsafePartial $ unsafeIndex _data 2
  in
    { r: red, g: green, b: blue, a: 1.0 }

colourMapDataToFunctionRefined :: Array (Array Number) -> Number -> { r :: Number, g :: Number, b :: Number, a :: Number }
colourMapDataToFunctionRefined dataArray val =
  let
    scaledVal =
      Number.max (Number.min 255.0 $ 255.0 * val) 0.0

    roundedScaledVal =
      Int.trunc scaledVal

    scaledValIsInt =
      Int.toNumber roundedScaledVal - scaledVal == 0.0
  in
    if scaledValIsInt then
      colourMapDataToFunction dataArray roundedScaledVal

    else
      let
        intLessThan =
          Int.floor scaledVal

        intGreaterThan =
          Int.ceil scaledVal

        lowerData =
          unsafePartial $ unsafeIndex dataArray intLessThan

        lowerRed =
          unsafePartial $ unsafeIndex lowerData 0

        lowerGreen =
          unsafePartial $ unsafeIndex lowerData 1

        lowerBlue =
          unsafePartial $ unsafeIndex lowerData 2

        upperData =
          unsafePartial $ unsafeIndex dataArray intGreaterThan

        upperRed =
          unsafePartial $ unsafeIndex upperData 0

        upperGreen =
          unsafePartial $ unsafeIndex upperData 1

        upperBlue =
          unsafePartial $ unsafeIndex upperData 2

        calculateWeightedMidPoint newStart newEnd =
          let
            lowerDiff =
              scaledVal - Int.toNumber intLessThan

            initialRange =
              intGreaterThan - intLessThan

            finalRange =
              newEnd - newStart
          in
            (lowerDiff * finalRange) / Int.toNumber initialRange + newEnd
      in
        { r: calculateWeightedMidPoint lowerRed upperRed
        , g: calculateWeightedMidPoint lowerGreen upperGreen
        , b: calculateWeightedMidPoint lowerBlue upperBlue
        , a: 1.0
        }
