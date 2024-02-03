module Fourier (fourierNumbersMagSquared) where

import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as ArrayST
import Data.Number (pow)
import FFT as FFT
import Partial.Unsafe (unsafePartial)

fourierNumbersMagSquared :: FFT.FFT -> FFT.RealArray -> Array Number
fourierNumbersMagSquared fft arr =
  let
    FFT.ComplexArray transformedArray =
      FFT.realTransform fft arr
  in
    ArrayST.run do
      arrST <- FFT.newUnsafe (Array.length transformedArray / 2)
      ST.for 0 (Array.length transformedArray / 2)
        ( \n ->
            let
              re =
                unsafePartial $ Array.unsafeIndex transformedArray (2 * n)

              im =
                unsafePartial $ Array.unsafeIndex transformedArray (2 * n + 1)
            in
              ArrayST.poke n (pow re 2.0 + pow im 2.0) arrST
        )
      pure arrST
