module Main where

import DataTypes
import Effect.Timer
import Prelude

import Colourmaps.Viridis as Viridis
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Maybe.Trans as MaybeT
import Control.Monad.ST (for)
import Data.Array (null)
import Data.Array as Array
import Data.Array.ST as ArrayST
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (Uint8Array)
import Data.ArrayBuffer.Types as ABT
import Data.Either (Either(..))
import Data.Float32 as Float32
import Data.Foldable (for_, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (power)
import Data.Number as Number
import Data.Semigroup.Foldable (minimum)
import Data.Tuple (Tuple(..), fst, snd)
import Data.UInt as Uint
import Data.Unfoldable (class Unfoldable, none)
import Deku.Attribute (Attribute, Cb, cb, cb')
import Deku.Attribute as DAttr
import Deku.Control as DC
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks as DH
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import FFT (FFT, RealArray(..), fftSize, makeFFT)
import FFT.Internal.Array (newUnsafe)
import FRP.Poll (Poll)
import File (stream, getReader, read)
import Fourier (fourierNumbersMagSquared)
import Graphics.Canvas (Context2D, createImageDataWith, getCanvasElementById, getContext2D, putImageData, setCanvasDimensions)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, target)
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileList (FileList, items)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.PointerEvent.PointerEvent (PointerEvent)
import Web.Streams.Reader (Reader)
import Web.UIEvent.FocusEvent (toEvent, FocusEvent)

klassList :: forall r. Array (Tuple (Poll String) Boolean) -> Poll (Attribute (klass :: String | r))
klassList arr =
  DA.klass $
    Array.foldl (\acc cur -> (\a b -> a <> " " <> b) <$> acc <*> cur) (pure "") (fst <$> Array.filter snd arr)

klassList_ :: forall r. Array (Tuple String Boolean) -> Poll (Attribute (klass :: String | r))
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
            [ klassList_ [ Tuple "pf-v5-c-card" true, Tuple "pf-m-full-height" true ]
            , DA.style_ "height: 100%; width: 100%;"
            ]
            [ DD.div
                [ DA.klass_ "pf-v5-c-card__body"
                , DA.style_ "position: relative;"
                ]
                [ DD.canvas
                    [ DA.id_ "canvas"
                    , DA.style_ "max-width: 50%; max-height: 100%; margin-left: auto; margin-right: auto; display: block;"
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
    Tuple setHeatmapType heatmapType <- DH.useState Viridis

    let
      onMinus :: Poll (PointerEvent -> Effect Unit)
      onMinus =
        averagingFactor <#> \n _ ->
          if n <= 1 then pure unit else setAveragingFactor (n / 2)

      onPlus :: Poll (PointerEvent -> Effect Unit)
      onPlus =
        averagingFactor <#> \n _ ->
          if n >= 64 then pure unit else setAveragingFactor (n * 2)

      updateAverage str =
        case Int.fromString str of
          Nothing -> pure unit
          Just n -> setAveragingFactor $ minimum $ Tuple 64 $ Int.pow 2 <<< Int.round <<< log2 $ Int.toNumber n

    DD.form
      [ klassList_ [ Tuple "pf-v5-c-form" true, Tuple "pf-m-horizontal" true ]
      , DA.novalidate_ ""
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
                            [ DL.click_ $ \_ -> setDataType dt ]
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
                              , DA.xtype_ "button"
                              ]
                              [ DD.span [ DA.klass_ "pf-v5-c-number-input__icon" ]
                                  [ DC.text_ "-" ]
                              ]
                          ]
                      , DD.div [ DA.klass_ "pf-v5-c-input-group__item" ]
                          [ DD.span [ DA.klass_ "pf-v5-c-form-control" ]
                              [ DD.input
                                  [ onValueBlur $ pure updateAverage
                                  , DA.value $ averagingFactor <#> show
                                  ]
                                  []
                              ]
                          ]
                      , DD.div [ DA.klass_ "pf-v5-c-input-group__item" ]
                          [ DD.button
                              [ klassList_ [ Tuple "pf-v5-c-button" true, Tuple "pf-m-control" true ]
                              , DL.click onPlus
                              , DA.xtype_ "button"
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
                  [ onFileUpload $ processFile <$> averagingFactor <*> dataType <*> heatmapType
                  , DA.xtype_ "file"
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
                  [ DD.select [] $
                      heatmapTypes <#> \t ->
                        DD.option [ DL.click_ $ \_ -> setHeatmapType t ] [ DC.text_ $ show t ]
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
  _ <- MaybeT.runMaybeT do
    -- Get file information
    file <- hoistMaybe mFile
    let
      fileByteSize = File.size file
      pixelCount = countPixels dataType fileByteSize
      fileStream = stream file
      fft = makeFFT 1024

    -- Get canvas data, configure its dimensions and get the
    -- underlying context.
    canvas <- MaybeT $ getCanvasElementById "canvas"
    liftEffect $ setCanvasDimensions canvas { height: Int.toNumber <<< Int.floor $ pixelCount / (Int.toNumber averagingValue * 1024.0), width: 1024.0 }
    ctx <- liftEffect $ getContext2D canvas

    liftEffect <<< runAff_
      ( case _ of
          Left err -> Console.logShow err
          Right _ -> pure unit
      ) $ do

      reader <- getReader fileStream
      _ <- read reader $ runFileStream { ctx, dataType, fft, row: 0, reader, heatmapType, averagingValue }
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
  :: { fft :: FFT, ctx :: Context2D, row :: Int, dataType :: DataType, reader :: Reader File, heatmapType :: HeatmapType, averagingValue :: Int }
  -> { done :: Boolean, value :: Uint8Array }
  -> Effect Unit
runFileStream { fft, ctx, row, dataType, reader, heatmapType, averagingValue } { done, value } = do
  if done then Console.log "Stream done"
  else
    do
      numberArray <- arrayBufferToEffArray dataType $ AB.buffer value
      newRow <- plotArray fft row ctx averagingValue heatmapType [] numberArray
      runAff_
        ( case _ of
            Left err -> Console.logShow err
            Right _ -> pure unit
        ) $ read reader $ runFileStream { fft, ctx, dataType, reader, row: newRow, heatmapType, averagingValue }

log10 :: Number -> Number
log10 x = (Number.log x) / Number.ln10

log2 :: Number -> Number
log2 x = (Number.log x) / Number.ln2

plotArray :: FFT -> Int -> Context2D -> Int -> HeatmapType -> Array Number -> Array Number -> Effect Int
-- This is the main exit point of the function.
plotArray _ row _ _ _ [] [] = pure row

--
plotArray fft row ctx averagingValue heatmapType [] next = do
  let { before, after } = averageArray next averagingValue 1024
  plotArray fft row ctx averagingValue heatmapType before after
plotArray fft row ctx averagingValue heatmapType toPlot next = do
  let arrLen = Array.length toPlot
  if arrLen /= 1024 && arrLen /= 0 then pure row
  else do
    plot1024Numbers { fft, ctx, row, col: 0, heatmapType } toPlot
    let { before, after } = averageArray next averagingValue 1024
    plotArray fft (row + 1) ctx averagingValue heatmapType before after

plot1024Numbers :: { fft :: FFT, ctx :: Context2D, row :: Int, col :: Int, heatmapType :: HeatmapType } -> Array Number -> Effect Unit
plot1024Numbers { fft, ctx, row, col, heatmapType } arr =
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
              getColourmap heatmapType x
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
      arrView :: ABT.ArrayView ABT.Uint8Clamped <- AB.fromArray colourArray
      imageData <- createImageDataWith arrView (1024)
      putImageData ctx imageData 0.0 rowPx
      _ <- setTimeout 0 (pure unit)
      pure unit

-- Extra stuff
onFileUpload :: forall t r. Unfoldable t => Poll (t File -> Effect Unit) -> Poll (Attribute (change :: Event | r))
onFileUpload pFunc =
  DL.change $ pFunc <#> \f ->
    ( target
        >=> HTMLInputElement.fromEventTarget
        >=>
          HTMLInputElement.files >>> unsafePerformEffect
    ) >>> maybe none items >>> f

onValueChange :: forall r. Poll (String -> Effect Unit) -> Poll (Attribute (value :: String, change :: Event | r))
onValueChange pFunc =
  DL.change $ pFunc <#> \f e ->
    case target >=> HTMLInputElement.fromEventTarget $ e of
      Nothing -> pure unit
      Just inEl -> do
        val <- HTMLInputElement.value inEl
        f val

onValueBlur :: forall r. Poll (String -> Effect Unit) -> Poll (Attribute (value :: String, blur :: FocusEvent | r))
onValueBlur pFunc =
  DL.blur $ pFunc <#> \f e ->
    case target >=> HTMLInputElement.fromEventTarget $ toEvent e of
      Nothing -> pure unit
      Just inEl -> do
        val <- HTMLInputElement.value inEl
        f val

averageArray :: Array Number -> Int -> Int -> { before :: Array Number, after :: Array Number }
averageArray nums 1 length = Array.splitAt length nums
averageArray nums factor length =
  if Array.length nums < length then { before: [], after: [] }
  else
    let
      indexFactors = (*) factor <$> Array.range 0 (factor - 1) :: Array Int
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
