module Main where

import DataTypes (DataType(..), HeatmapType(..), dataTypeBytes, dataTypes, getColourmap, heatmapTypes)
import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Maybe.Trans as MaybeT
import Control.Monad.ST (for)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Control.Monad.State.Trans (get, modify_)
import Data.Array as Array
import Data.Array.ST as ArrayST
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (Uint8Array)
import Data.ArrayBuffer.Types as ABT
import Data.Either (Either(..))
import Data.Float32 as Float32
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.Semigroup.Foldable (minimum)
import Data.Tuple (Tuple(..), fst, snd)
import Data.UInt as Uint
import Data.Unfoldable (class Unfoldable, none)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks as DH
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import FFT (FFT, RealArray(..), makeFFT)
import FFT.Internal.Array (newUnsafe)
import FRP.Poll (Poll)
import File (stream, getReader, read)
import Fourier (fourierNumbersMagSquared)
import Graphics.Canvas (Context2D, createImageDataWith, getCanvasElementById, getContext2D, putImageData, setCanvasDimensions)
import Partial.Unsafe (unsafePartial)
import Web.Event.Event (Event, target)
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileList (items)
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
            , DA.style_ "height: 100%; width: 100%; position: relative; overflow-y: auto;"
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
    Tuple setFile file <- DH.useState (Nothing :: Maybe File)

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
--processFile <$> averagingFactor <*> dataType <*> heatmapType
                  [ onFileUpload <<< pure $ case _ of
                       Just f -> setFile $ Just f
                       Nothing -> pure unit
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
      , DD.div
        [ DA.klass_ "pf-v5-c-form__group" ]
        [ DD.button
          [ klassList [ Tuple (pure "pf-v5-c-button") true, Tuple (pure "pf-m-primary") true ]
          , DA.xtype_ "button"
          , DL.click $ (\a d h f _ -> processFile a d h f) <$> averagingFactor <*> dataType <*> heatmapType <*> file
          ]
          [ DC.text_ "Go!"]
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
    liftEffect $ setCanvasDimensions canvas { height: min 32767.0 $ Int.toNumber <<< Int.floor $ pixelCount / (Int.toNumber averagingValue * 1024.0), width: 1024.0 }
    ctx <- liftEffect $ getContext2D canvas

    liftEffect <<< runAff_
      ( case _ of
          Left err -> Console.logShow err
          Right _ -> pure unit
      ) $ do

      reader <- getReader fileStream
      _ <- read reader $ \a -> runStateT (runFileStream a) { ctx, dataType, fft, row: 0, reader, heatmapType, averagingValue }
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

type State = { fft :: FFT, ctx :: Context2D, row :: Int, dataType :: DataType, reader :: Reader File, heatmapType :: HeatmapType, averagingValue :: Int }

runFileStream
  :: { done :: Boolean, value :: Uint8Array }
  -> StateT State Effect Unit
runFileStream { done, value } = do
  if done then liftEffect $ Console.log "Stream done"
  else
    do
      state <- get
      numberArray <- liftEffect $ arrayBufferToEffArray state.dataType $ AB.buffer value
      newRow <- plotArray [] numberArray
      modify_ $ _ { row = newRow }
      liftEffect
        $ runAff_
            ( case _ of
                Left err -> liftEffect $ Console.logShow err
                Right _ -> pure unit
            )
        $ read state.reader
        $ \a -> evalStateT (runFileStream a) (state { row = newRow })

log10 :: Number -> Number
log10 x = (Number.log x) / Number.ln10

log2 :: Number -> Number
log2 x = (Number.log x) / Number.ln2

plotArray :: Array Number -> Array Number -> StateT State Effect Int
plotArray [] [] = do
  -- This is the main exit point of the function.
  { row } <- get
  pure row
plotArray [] next = do
  { averagingValue } <- get
  let { before, after } = averageArray next averagingValue 1024
  plotArray before after
plotArray toPlot next = do
  state <- get
  let arrLen = Array.length toPlot
  if arrLen /= 1024 && arrLen /= 0 then pure state.row
  else do
    liftEffect $ plot1024Numbers state toPlot
    let { before, after } = averageArray next state.averagingValue 1024
    modify_ $ \s -> s { row = s.row + 1 }
    plotArray before after

plot1024Numbers :: forall r. { fft :: FFT, ctx :: Context2D, row :: Int, heatmapType :: HeatmapType | r } -> Array Number -> Effect Unit
plot1024Numbers { fft, ctx, row, heatmapType } arr =
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
averageArray nums factor length =
  let
    { before, after } = Array.splitAt length nums
  in
    go before after factor
  where
  go before after 1 =
    { before: before <#> \k -> k / (Int.toNumber factor), after }
  go bef aft n =
    let
      { before, after } = Array.splitAt length aft
      newBefore = Array.zipWith (+) bef before
    in
      go newBefore after (n - 1)
