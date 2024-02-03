module Main (main) where

import DataTypes
import Effect.Timer
import Graphics.Canvas (Context2D, createImageDataWith, getCanvasElementById, getContext2D, putImageData, setCanvasDimensions)
import Prelude

import Colourmaps.Viridis as Viridis
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Maybe.Trans as MaybeT
import Data.Array (null)
import Data.Array as Array
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (Uint8Array)
import Data.ArrayBuffer.Types as ABT
import Data.Float32 as Float32
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.Tuple (Tuple(..), fst, snd)
import Data.UInt as Uint
import Data.Unfoldable (class Unfoldable, none)
import Deku.Attribute (class Attr, Attribute, Cb, cb, cb')
import Deku.Attribute as DAttr
import Deku.Attributes as DA
import Deku.Control as DC
import Deku.DOM as DD
import Deku.Do as Deku
import Deku.Hooks as DH
import Deku.Listeners as DL
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import FFT (FFT, RealArray(..), fftSize, makeFFT)
import FRP.Event (Event)
import File (stream, getReader, read)
import Fourier (fourierNumbersMagSquared)
import Web.Event.Event (target)
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileList (FileList, items)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.Streams.Reader (Reader)

klassList :: forall e. Attr e DD.Class String => Array (Tuple (Event String) Boolean) -> Event (Attribute e)
klassList arr =
  DA.klass $
    Array.foldl (\acc cur -> (\a b -> a <> " " <> b) <$> acc <*> cur) (pure "") (fst <$> Array.filter snd arr)

klassList_ :: forall e. Attr e DD.Class String => Array (Tuple String Boolean) -> Event (Attribute e)
klassList_ arr =
  DA.klass_ $
    Array.foldl (\acc cur -> acc <> " " <> cur) "" (fst <$> Array.filter snd arr)

main :: Effect Unit
main = runInBody Deku.do
  DD.div
    [ klassList_ [ Tuple "pf-v5-c-page" true ] ]
    [ DD.header
        [ klassList_ [ Tuple "pf-v5-c-masthead" true ] ]
        [ DD.div
            [ klassList_ [ Tuple "pf-v5-c-masthead__main" true ] ]
            []
        , DD.div
            [ klassList_ [ Tuple "pf-v5-c-masthead__content" true ] ]
            [ DC.text_ "Speccy" ]
        ]
    , DD.main
        [ DA.klass_ "pf-v5-c-page__main" ]
        [ DD.div
            [ klassList_ [ Tuple "pf-v5-c-card" true ] ]
            [ DD.div
                [ DA.klass_ "pf-v5-c-card__body" ]
                [ speccyForm ]
            ]
        , DD.div
            [ klassList_ [ Tuple "pf-v5-c-card" true, Tuple "pf-m-full-height" true ] ]
            [ DD.div
                [ DA.klass_ "pf-v5-c-card__body" ]
                [ DD.canvas
                    [ DA.id_ "canvas"
                    , DA.style_ "width: 100%; height: 100%;"
                    ]
                    []
                ]
            ]
        ]
    ]
  where
  speccyForm = Deku.do
    Tuple setDataType dataType <- DH.useState Uint8
    Tuple setAveragingFactor averagingFactor <- DH.useState 1
    Tuple setFile file <- DH.useState (Nothing :: Maybe File)
    Tuple setHeatmapType heatmapType <- DH.useState ViridisType

    let
      onMinus =
        averagingFactor <#> \n ->
          if n <= 1 then pure unit else setAveragingFactor (n - 1)

      updateAverage str =
        case Int.fromString str of
          Nothing -> pure unit
          Just n -> setAveragingFactor n

    DD.form
      [ klassList_ [ Tuple "pf-v5-c-form" true, Tuple "pf-m-horizontal" true ]
      , pure $ DAttr.attr NoValidate true
      ]
      [ DD.div
          [ DA.klass_ "pf-v5-c-form__group" ]
          [ DD.div [ DA.klass_ "pf-v5-c-form__group-label" ]
              [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ]
                  [ DC.text_ "Data format" ]
              ]
          , DD.div [ DA.klass_ "pf-v5-c-form_group-control" ]
              [ DD.span [ DA.klass_ "pf-v5-c-form-control" ]
                  [ DD.select []
                      $ dataTypes <#> \dt ->
                          DD.option
                            [ DL.click_ $ setDataType dt ]
                            [ DC.text_ <<< show $ dt ]
                  ]
              ]
          ]
      , DD.div
          [ DA.klass_ "pf-v5-c-form__group" ]
          [ DD.div [ DA.klass_ "pf-v5-c-form__group-label" ]
              [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ]
                  [ DC.text_ "Averaging factor" ]
              ]
          , DD.div [ DA.klass_ "pf-v5-c-form_group-control" ]
              [ DD.div [ DA.klass_ "pf-v5-c-number-input" ]
                  [ DD.div [ DA.klass_ "pf-v5-c-input-group" ]
                      [ DD.div [ DA.klass_ "pf-v5-c-input-group__item" ]
                          [ DD.button
                              [ klassList_ [ Tuple "pf-v5-c-button" true, Tuple "pf-m-control" true ]
                              , DL.click onMinus
                              , pure $ DAttr.attr Type_ "button"
                              ]
                              [ DD.span [ DA.klass_ "pf-v5-c-number-input__icon" ]
                                  [ DC.text_ "-" ]
                              ]
                          ]
                      , DD.div [ DA.klass_ "pf-v5-c-input-group__item" ]
                          [ DD.span [ DA.klass_ "pf-v5-c-form-control" ]
                              [ DD.input
                                  [ averagingFactor <#> \n -> DAttr.attr Value $ show n
                                  , DL.textInput_ updateAverage
                                  ]
                                  []
                              ]
                          ]
                      , DD.div [ DA.klass_ "pf-v5-c-input-group__item" ]
                          [ DD.button
                              [ klassList_ [ Tuple "pf-v5-c-button" true, Tuple "pf-m-control" true ]
                              , DL.click $ averagingFactor <#> \n -> setAveragingFactor (n + 1)
                              , pure $ DAttr.attr Type_ "button"
                              ]
                              [ DD.span [ DA.klass_ "pf-v5-c-number-input__icon" ]
                                  [ DC.text_ "+" ]
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      , DD.div
          [ DA.klass_ "pf-v5-c-form__group" ]
          [ DD.div [ DA.klass_ "pf-v5-c-form__group-label" ]
              [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ]
                  [ DC.text_ "Selected file" ]
              ]
          , DD.div [ DA.klass_ "pf-v5-c-form_group-control" ]
              [ DD.input
                  [ fileUpload $ processFile <$> averagingFactor <*> dataType <*> heatmapType
                  , pure $ DAttr.attr Type_ "file"
                  ]
                  []
              ]
          ]
      , DD.div
          [ DA.klass_ "pf-v5-c-form__group" ]
          [ DD.div [ DA.klass_ "pf-v5-c-form__group-label" ]
              [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ]
                  [ DC.text_ "Heatmap type" ]
              ]
          , DD.div [ DA.klass_ "pf-v5-c-form_group-control" ]
              [ DD.span [ DA.klass_ "pf-v5-c-form-control" ]
                  [ DD.select []
                      [ DD.option [] [ DC.text_ "foo" ] ]
                  ]
              ]
          ]
      ]

-- Spectrum time

hoistMaybe :: forall m a. Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT <<< pure

countPixels :: DataType -> Number -> Number
countPixels dataType fileByteSize =
  fileByteSize / (dataTypeBytes dataType)

processFile :: Int -> DataType -> HeatmapType -> Maybe File -> Effect Unit
processFile averagingValue dataType heatmapType mFile = do
  Console.log "STARTING PROCESS FILE"
  _ <- MaybeT.runMaybeT do
    -- Get file information
    file <- hoistMaybe mFile
    liftEffect $ Console.log "HOISTED FILE"
    let
      fileByteSize = File.size file
      pixelCount = countPixels dataType fileByteSize
      fileStream = stream file
      fft = makeFFT 1024

    -- Get canvas data, configure its dimensions and get the
    -- underlying context.
    canvas <- MaybeT $ getCanvasElementById "canvas"
    liftEffect $ setCanvasDimensions canvas { height: pixelCount / 1024.0, width: 1024.0 }
    ctx <- liftEffect $ getContext2D canvas

    liftEffect <<< launchAff_ $ do
      reader <- getReader fileStream
      _ <- read reader $ runFileStream { ctx, dataType, fft, row: 0, reader, heatmapType }
      pure unit

    pure unit
  pure unit

arrayBufferToEffArray :: DataType -> ABT.ArrayBuffer -> Effect (Array Number)
arrayBufferToEffArray dataType arrBuff =
  case dataType of
    Uint8 -> do
      arrView <- (AB.whole arrBuff :: Effect (ABT.ArrayView ABT.Uint8))
      arr <- AB.toArray arrView
      pure $ Uint.toNumber <$> arr

    Int8 -> do
      arrView <- (AB.whole arrBuff :: Effect (ABT.ArrayView ABT.Int8))
      arr <- AB.toArray arrView
      pure $ Int.toNumber <$> arr

    Uint16 -> do
      arrView <- (AB.whole arrBuff :: Effect (ABT.ArrayView ABT.Uint16))
      arr <- AB.toArray arrView
      pure $ Uint.toNumber <$> arr

    Int16 -> do
      arrView <- (AB.whole arrBuff :: Effect (ABT.ArrayView ABT.Int16))
      arr <- AB.toArray arrView
      pure $ Int.toNumber <$> arr

    Uint32 -> do
      arrView <- (AB.whole arrBuff :: Effect (ABT.ArrayView ABT.Uint32))
      arr <- AB.toArray arrView
      pure $ Uint.toNumber <$> arr

    Int32 -> do
      arrView <- (AB.whole arrBuff :: Effect (ABT.ArrayView ABT.Int32))
      arr <- AB.toArray arrView
      pure $ Int.toNumber <$> arr

    Float32 -> do
      arrView <- (AB.whole arrBuff :: Effect (ABT.ArrayView ABT.Float32))
      arr <- AB.toArray arrView
      pure $ Float32.toNumber <$> arr

    Float64 -> do
      arrView <- (AB.whole arrBuff :: Effect (ABT.ArrayView ABT.Float64))
      AB.toArray arrView

runFileStream
  :: { fft :: FFT, ctx :: Context2D, row :: Int, dataType :: DataType, reader :: Reader File, heatmapType :: HeatmapType }
  -> { done :: Boolean, value :: Uint8Array }
  -> Effect Unit
runFileStream { fft, ctx, row, dataType, reader, heatmapType } { done, value } = do
  if done then Console.log "Stream done"
  else
    do
      numberArray <- arrayBufferToEffArray dataType $ AB.buffer value
      Console.log $ "size of array: " <> show (Array.length numberArray)
      newRow <- plotArray fft row ctx [] numberArray
      launchAff_ $ read reader $ runFileStream { fft, ctx, dataType, reader, row: newRow - 10, heatmapType }

log10 :: Number -> Number
log10 x = (Number.log x) / Number.ln10

plotArray :: FFT -> Int -> Context2D -> Array Number -> Array Number -> Effect Int
plotArray _ row _ _ [] = pure $ row
plotArray fft row ctx [] next = do
  let { before, after } = Array.splitAt 1024 next
  plotArray fft row ctx before after
plotArray fft row ctx toPlot next = do
  let arrLen = Array.length toPlot
  if arrLen /= 1024 && arrLen /= 0 then pure $ row
  else do
    Console.log $ "ROW: " <> show row
    plot1024Numbers { fft, ctx, row, col: 0 } toPlot
    let { before, after } = Array.splitAt 1024 next
    plotArray fft (row + 1) ctx before after

plot1024Numbers :: { fft :: FFT, ctx :: Context2D, row :: Int, col :: Int } -> Array Number -> Effect Unit
plot1024Numbers { fft, ctx, row, col } arr =
  let
    fourieredArray =
      fourierNumbersMagSquared fft $ RealArray arr

    maxVal =
      Array.foldl Number.max 0.0 fourieredArray

    log10Max =
      log10 maxVal

    scaledArray =
      (\x -> (log10 x) / log10Max) <$> fourieredArray

    colourArray =
      scaledArray >>=
        \x ->
          let
            colour =
              Viridis.viridisColourMapRefined x
          in
            [ Uint.round $ 255.0 * colour.r
            , Uint.round $ 255.0 * colour.g
            , Uint.round $ 255.0 * colour.b
            , Uint.round $ 255.0 * colour.a
            ]
    rowPx =
      Int.toNumber row
  in
    do
      Console.log $ "normal arr"
      Console.logShow arr
      Console.logShow fourieredArray
      arrView :: ABT.ArrayView ABT.Uint8Clamped <- AB.fromArray colourArray
      imageData <- createImageDataWith arrView (1024)
      putImageData ctx imageData 0.0 rowPx

-- Extra stuff

data NoValidate = NoValidate

instance Attr DD.Form_ NoValidate Boolean where
  attr NoValidate value = DAttr.unsafeAttribute { key: "novalidate", value: if value then DAttr.prop' "" else DAttr.Unset' }

data Type_ = Type_

instance Attr a Type_ String where
  attr Type_ value = DAttr.unsafeAttribute { key: "type", value: DAttr.prop' value }

data FileUpload = FileUpload

instance Attr DD.Input_ FileUpload Cb where
  attr FileUpload value = DAttr.unsafeAttribute { key: "change", value: cb' value }

fileUpload :: forall t. Unfoldable t => Event (t File -> Effect Unit) -> Event (Attribute DD.Input_)
fileUpload =
  map $ \f -> DAttr.attr DD.OnChange
    ( cb $
        ( target
            >=> HTMLInputElement.fromEventTarget
            >=>
              HTMLInputElement.files >>> unsafePerformEffect
        ) >>> maybe none items
          >>> f
    )

data Value = Value

instance Attr DD.Input_ Value String where
  attr Value value = DAttr.unsafeAttribute { key: "value", value: DAttr.prop' value }
