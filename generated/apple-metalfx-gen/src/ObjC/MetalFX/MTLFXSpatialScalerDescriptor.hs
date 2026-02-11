{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A set of properties that configure a spatial scaling effect, and a factory method that creates the effect.
--
-- Generated bindings for @MTLFXSpatialScalerDescriptor@.
module ObjC.MetalFX.MTLFXSpatialScalerDescriptor
  ( MTLFXSpatialScalerDescriptor
  , IsMTLFXSpatialScalerDescriptor(..)
  , newSpatialScalerWithDevice
  , newSpatialScalerWithDevice_compiler
  , supportsMetal4FX
  , supportsDevice
  , colorTextureFormat
  , setColorTextureFormat
  , outputTextureFormat
  , setOutputTextureFormat
  , inputWidth
  , setInputWidth
  , inputHeight
  , setInputHeight
  , outputWidth
  , setOutputWidth
  , outputHeight
  , setOutputHeight
  , colorProcessingMode
  , setColorProcessingMode
  , newSpatialScalerWithDeviceSelector
  , newSpatialScalerWithDevice_compilerSelector
  , supportsMetal4FXSelector
  , supportsDeviceSelector
  , colorTextureFormatSelector
  , setColorTextureFormatSelector
  , outputTextureFormatSelector
  , setOutputTextureFormatSelector
  , inputWidthSelector
  , setInputWidthSelector
  , inputHeightSelector
  , setInputHeightSelector
  , outputWidthSelector
  , setOutputWidthSelector
  , outputHeightSelector
  , setOutputHeightSelector
  , colorProcessingModeSelector
  , setColorProcessingModeSelector

  -- * Enum types
  , MTLFXSpatialScalerColorProcessingMode(MTLFXSpatialScalerColorProcessingMode)
  , pattern MTLFXSpatialScalerColorProcessingModePerceptual
  , pattern MTLFXSpatialScalerColorProcessingModeLinear
  , pattern MTLFXSpatialScalerColorProcessingModeHDR

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalFX.Internal.Classes
import ObjC.MetalFX.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a spatial scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the spatial scaler. - Returns:    A new spatial scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newSpatialScalerWithDevice:@
newSpatialScalerWithDevice :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> RawId -> IO RawId
newSpatialScalerWithDevice mtlfxSpatialScalerDescriptor  device =
    fmap (RawId . castPtr) $ sendMsg mtlfxSpatialScalerDescriptor (mkSelector "newSpatialScalerWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | Creates a spatial scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the spatial scaler.    - compiler: A compiler instance this method can use to build pipeline state objects. - Returns:    A new spatial scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newSpatialScalerWithDevice:compiler:@
newSpatialScalerWithDevice_compiler :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> RawId -> RawId -> IO RawId
newSpatialScalerWithDevice_compiler mtlfxSpatialScalerDescriptor  device compiler =
    fmap (RawId . castPtr) $ sendMsg mtlfxSpatialScalerDescriptor (mkSelector "newSpatialScalerWithDevice:compiler:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId compiler) :: Ptr ())]

-- | Queries whether a Metal device supports spatial scaling compatible with Metal 4.
--
-- - Parameters:    - device: The GPU device for which this methods tests support.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports spatial scaling with             Metal 4, <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsMetal4FX:@
supportsMetal4FX :: RawId -> IO Bool
supportsMetal4FX device =
  do
    cls' <- getRequiredClass "MTLFXSpatialScalerDescriptor"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsMetal4FX:") retCULong [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | Returns a Boolean value that indicates whether the spatial scaler works with a GPU.
--
-- - Parameters:    - device: An ``MTLDevice`` instance that represents a GPU.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports spatial scaling,            <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsDevice:@
supportsDevice :: RawId -> IO Bool
supportsDevice device =
  do
    cls' <- getRequiredClass "MTLFXSpatialScalerDescriptor"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDevice:") retCULong [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | The pixel format of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- colorTextureFormat@
colorTextureFormat :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CInt
colorTextureFormat mtlfxSpatialScalerDescriptor  =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "colorTextureFormat") retCInt []

-- | The pixel format of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setColorTextureFormat:@
setColorTextureFormat :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CInt -> IO ()
setColorTextureFormat mtlfxSpatialScalerDescriptor  value =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "setColorTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the output texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- outputTextureFormat@
outputTextureFormat :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CInt
outputTextureFormat mtlfxSpatialScalerDescriptor  =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "outputTextureFormat") retCInt []

-- | The pixel format of the output texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputTextureFormat:@
setOutputTextureFormat :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CInt -> IO ()
setOutputTextureFormat mtlfxSpatialScalerDescriptor  value =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "setOutputTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The width of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- inputWidth@
inputWidth :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CULong
inputWidth mtlfxSpatialScalerDescriptor  =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "inputWidth") retCULong []

-- | The width of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setInputWidth:@
setInputWidth :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CULong -> IO ()
setInputWidth mtlfxSpatialScalerDescriptor  value =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "setInputWidth:") retVoid [argCULong value]

-- | The height of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- inputHeight@
inputHeight :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CULong
inputHeight mtlfxSpatialScalerDescriptor  =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "inputHeight") retCULong []

-- | The height of the input color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setInputHeight:@
setInputHeight :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CULong -> IO ()
setInputHeight mtlfxSpatialScalerDescriptor  value =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "setInputHeight:") retVoid [argCULong value]

-- | The width of the output color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- outputWidth@
outputWidth :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CULong
outputWidth mtlfxSpatialScalerDescriptor  =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "outputWidth") retCULong []

-- | The width of the output color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputWidth:@
setOutputWidth :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CULong -> IO ()
setOutputWidth mtlfxSpatialScalerDescriptor  value =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "setOutputWidth:") retVoid [argCULong value]

-- | The height of the output color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- outputHeight@
outputHeight :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO CULong
outputHeight mtlfxSpatialScalerDescriptor  =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "outputHeight") retCULong []

-- | The height of the output color texture for the spatial scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputHeight:@
setOutputHeight :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> CULong -> IO ()
setOutputHeight mtlfxSpatialScalerDescriptor  value =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "setOutputHeight:") retVoid [argCULong value]

-- | The color space of the input color texture for the spatial scaler you create with this descriptor.
--
-- This property's default value is ``MTLFXSpatialScalerColorProcessingMode/MTLFXSpatialScalerColorProcessingModePerceptual``.
--
-- ObjC selector: @- colorProcessingMode@
colorProcessingMode :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> IO MTLFXSpatialScalerColorProcessingMode
colorProcessingMode mtlfxSpatialScalerDescriptor  =
    fmap (coerce :: CLong -> MTLFXSpatialScalerColorProcessingMode) $ sendMsg mtlfxSpatialScalerDescriptor (mkSelector "colorProcessingMode") retCLong []

-- | The color space of the input color texture for the spatial scaler you create with this descriptor.
--
-- This property's default value is ``MTLFXSpatialScalerColorProcessingMode/MTLFXSpatialScalerColorProcessingModePerceptual``.
--
-- ObjC selector: @- setColorProcessingMode:@
setColorProcessingMode :: IsMTLFXSpatialScalerDescriptor mtlfxSpatialScalerDescriptor => mtlfxSpatialScalerDescriptor -> MTLFXSpatialScalerColorProcessingMode -> IO ()
setColorProcessingMode mtlfxSpatialScalerDescriptor  value =
    sendMsg mtlfxSpatialScalerDescriptor (mkSelector "setColorProcessingMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newSpatialScalerWithDevice:@
newSpatialScalerWithDeviceSelector :: Selector
newSpatialScalerWithDeviceSelector = mkSelector "newSpatialScalerWithDevice:"

-- | @Selector@ for @newSpatialScalerWithDevice:compiler:@
newSpatialScalerWithDevice_compilerSelector :: Selector
newSpatialScalerWithDevice_compilerSelector = mkSelector "newSpatialScalerWithDevice:compiler:"

-- | @Selector@ for @supportsMetal4FX:@
supportsMetal4FXSelector :: Selector
supportsMetal4FXSelector = mkSelector "supportsMetal4FX:"

-- | @Selector@ for @supportsDevice:@
supportsDeviceSelector :: Selector
supportsDeviceSelector = mkSelector "supportsDevice:"

-- | @Selector@ for @colorTextureFormat@
colorTextureFormatSelector :: Selector
colorTextureFormatSelector = mkSelector "colorTextureFormat"

-- | @Selector@ for @setColorTextureFormat:@
setColorTextureFormatSelector :: Selector
setColorTextureFormatSelector = mkSelector "setColorTextureFormat:"

-- | @Selector@ for @outputTextureFormat@
outputTextureFormatSelector :: Selector
outputTextureFormatSelector = mkSelector "outputTextureFormat"

-- | @Selector@ for @setOutputTextureFormat:@
setOutputTextureFormatSelector :: Selector
setOutputTextureFormatSelector = mkSelector "setOutputTextureFormat:"

-- | @Selector@ for @inputWidth@
inputWidthSelector :: Selector
inputWidthSelector = mkSelector "inputWidth"

-- | @Selector@ for @setInputWidth:@
setInputWidthSelector :: Selector
setInputWidthSelector = mkSelector "setInputWidth:"

-- | @Selector@ for @inputHeight@
inputHeightSelector :: Selector
inputHeightSelector = mkSelector "inputHeight"

-- | @Selector@ for @setInputHeight:@
setInputHeightSelector :: Selector
setInputHeightSelector = mkSelector "setInputHeight:"

-- | @Selector@ for @outputWidth@
outputWidthSelector :: Selector
outputWidthSelector = mkSelector "outputWidth"

-- | @Selector@ for @setOutputWidth:@
setOutputWidthSelector :: Selector
setOutputWidthSelector = mkSelector "setOutputWidth:"

-- | @Selector@ for @outputHeight@
outputHeightSelector :: Selector
outputHeightSelector = mkSelector "outputHeight"

-- | @Selector@ for @setOutputHeight:@
setOutputHeightSelector :: Selector
setOutputHeightSelector = mkSelector "setOutputHeight:"

-- | @Selector@ for @colorProcessingMode@
colorProcessingModeSelector :: Selector
colorProcessingModeSelector = mkSelector "colorProcessingMode"

-- | @Selector@ for @setColorProcessingMode:@
setColorProcessingModeSelector :: Selector
setColorProcessingModeSelector = mkSelector "setColorProcessingMode:"

