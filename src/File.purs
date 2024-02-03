module File (stream, getReader, read) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Web.File.File (File)
import Effect (Effect)
import Effect.Aff (Aff)
import Control.Promise (Promise, toAffE)
import Web.Streams.Reader (Reader)
import Web.Streams.ReadableStream (ReadableStream)

foreign import getReaderImpl :: ReadableStream File -> Effect (Promise (Reader File))

foreign import readImpl :: forall a. Reader File -> ({ done :: Boolean, value :: Uint8Array } -> Effect a) -> Effect (Promise a)

foreign import stream :: File -> ReadableStream File

getReader :: ReadableStream File -> Aff (Reader File)
getReader fileStream = toAffE $ getReaderImpl fileStream

read :: forall a. Reader File -> ({ done :: Boolean, value :: Uint8Array } -> Effect a) -> Aff a
read reader callback = toAffE $ readImpl reader callback
