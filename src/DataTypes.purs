module DataTypes where

import Prelude

import Data.Int (toNumber, ceil)

-- * Datatypes

data DataType
  = Uint8
  | Int8
  | Uint16
  | Int16
  | Uint32
  | Int32
  | Float32
  | Float64

derive instance eqDataTypes :: Eq DataType
derive instance ordDataTypes :: Ord DataType

dataTypes :: Array DataType
dataTypes =
  [ Uint8, Int8, Uint16, Int16, Uint32, Int32, Float32, Float64 ]

dataTypeBytes :: DataType -> Number
dataTypeBytes datatype =
  case datatype of
    Uint8 -> 1.0

    Int8 -> 1.0

    Uint16 -> 2.0

    Int16 -> 2.0

    Uint32 -> 4.0

    Int32 -> 4.0

    Float32 -> 4.0

    Float64 -> 8.0

instance Show DataType where
  show Uint8 = "Uint8"
  show Int8 = "Int8"
  show Uint16 = "Uint16"
  show Int16 = "Int16"
  show Uint32 = "Uint32"
  show Int32 = "Int32"
  show Float32 = "Float32"
  show Float64 = "Float64"

numberOfColumns :: { numberOfRows :: Int, fileBytes :: Int, dataType :: DataType } -> Int
numberOfColumns { numberOfRows, fileBytes, dataType } =
  let
    pixelCount =
      toNumber fileBytes / (dataTypeBytes dataType)
  in
    ceil $ pixelCount / toNumber numberOfRows

-- * Heatmap types

data HeatmapType
  = ViridisType
  | GreyType

instance Show HeatmapType where
  show ViridisType = "viridis"
  show GreyType = "grey"

heatmapTypes :: Array HeatmapType
heatmapTypes =
  [ ViridisType, GreyType ]
