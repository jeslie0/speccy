module Other where

import Prelude

import Control.Monad.ST (for)
import Data.Array as Array
import Data.Array.ST as ArrayST
import Data.Foldable (sum)
import Data.Int as Int
import FFT (newUnsafe)
import Partial.Unsafe (unsafePartial)

-- Statefule average
averageArray :: Array Number -> Int -> Int -> { before :: Array Number, after :: Array Number }
averageArray nums 1 length = Array.splitAt length nums
averageArray nums factor length =
  if Array.length nums < length then { before: [], after: [] }
  else
    let
      indexFactors = (*) length <$> Array.range 0 (factor - 1) :: Array Int
      { before, after } = Array.splitAt (factor * length) nums
      averagedBefore =
        unsafePartial $ ArrayST.run do
          x <- newUnsafe length
          for 0 length
            ( \n -> ArrayST.poke n
                ( (\k -> k / Int.toNumber factor)
                    <<< sum $ (\m -> Array.unsafeIndex before $ n + m) <$> indexFactors
                )
                x
            )
          pure x
    in
      { after, before: averagedBefore }
