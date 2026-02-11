{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLFXTemporalDenoisedScalerDescriptor@.
module ObjC.MetalFX.MTLFXTemporalDenoisedScalerDescriptor
  ( MTLFXTemporalDenoisedScalerDescriptor
  , IsMTLFXTemporalDenoisedScalerDescriptor(..)
  , newTemporalDenoisedScalerWithDevice
  , newTemporalDenoisedScalerWithDevice_compiler
  , supportedInputContentMinScaleForDevice
  , supportedInputContentMaxScaleForDevice
  , supportsMetal4FX
  , supportsDevice
  , colorTextureFormat
  , setColorTextureFormat
  , depthTextureFormat
  , setDepthTextureFormat
  , motionTextureFormat
  , setMotionTextureFormat
  , diffuseAlbedoTextureFormat
  , setDiffuseAlbedoTextureFormat
  , specularAlbedoTextureFormat
  , setSpecularAlbedoTextureFormat
  , normalTextureFormat
  , setNormalTextureFormat
  , roughnessTextureFormat
  , setRoughnessTextureFormat
  , specularHitDistanceTextureFormat
  , setSpecularHitDistanceTextureFormat
  , denoiseStrengthMaskTextureFormat
  , setDenoiseStrengthMaskTextureFormat
  , transparencyOverlayTextureFormat
  , setTransparencyOverlayTextureFormat
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
  , requiresSynchronousInitialization
  , setRequiresSynchronousInitialization
  , autoExposureEnabled
  , setAutoExposureEnabled
  , reactiveMaskTextureEnabled
  , setReactiveMaskTextureEnabled
  , reactiveMaskTextureFormat
  , setReactiveMaskTextureFormat
  , specularHitDistanceTextureEnabled
  , setSpecularHitDistanceTextureEnabled
  , denoiseStrengthMaskTextureEnabled
  , setDenoiseStrengthMaskTextureEnabled
  , transparencyOverlayTextureEnabled
  , setTransparencyOverlayTextureEnabled
  , newTemporalDenoisedScalerWithDeviceSelector
  , newTemporalDenoisedScalerWithDevice_compilerSelector
  , supportedInputContentMinScaleForDeviceSelector
  , supportedInputContentMaxScaleForDeviceSelector
  , supportsMetal4FXSelector
  , supportsDeviceSelector
  , colorTextureFormatSelector
  , setColorTextureFormatSelector
  , depthTextureFormatSelector
  , setDepthTextureFormatSelector
  , motionTextureFormatSelector
  , setMotionTextureFormatSelector
  , diffuseAlbedoTextureFormatSelector
  , setDiffuseAlbedoTextureFormatSelector
  , specularAlbedoTextureFormatSelector
  , setSpecularAlbedoTextureFormatSelector
  , normalTextureFormatSelector
  , setNormalTextureFormatSelector
  , roughnessTextureFormatSelector
  , setRoughnessTextureFormatSelector
  , specularHitDistanceTextureFormatSelector
  , setSpecularHitDistanceTextureFormatSelector
  , denoiseStrengthMaskTextureFormatSelector
  , setDenoiseStrengthMaskTextureFormatSelector
  , transparencyOverlayTextureFormatSelector
  , setTransparencyOverlayTextureFormatSelector
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
  , requiresSynchronousInitializationSelector
  , setRequiresSynchronousInitializationSelector
  , autoExposureEnabledSelector
  , setAutoExposureEnabledSelector
  , reactiveMaskTextureEnabledSelector
  , setReactiveMaskTextureEnabledSelector
  , reactiveMaskTextureFormatSelector
  , setReactiveMaskTextureFormatSelector
  , specularHitDistanceTextureEnabledSelector
  , setSpecularHitDistanceTextureEnabledSelector
  , denoiseStrengthMaskTextureEnabledSelector
  , setDenoiseStrengthMaskTextureEnabledSelector
  , transparencyOverlayTextureEnabledSelector
  , setTransparencyOverlayTextureEnabledSelector


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
import ObjC.Foundation.Internal.Classes

-- | Creates a denoiser scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the denoiser scaler. - Returns:    A denoiser scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newTemporalDenoisedScalerWithDevice:@
newTemporalDenoisedScalerWithDevice :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> RawId -> IO RawId
newTemporalDenoisedScalerWithDevice mtlfxTemporalDenoisedScalerDescriptor  device =
    fmap (RawId . castPtr) $ sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "newTemporalDenoisedScalerWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | Creates a denoiser scaler instance for a Metal device.
--
-- - Parameters:    - device: The Metal device that creates the denoiser scaler.    - compiler: A compiler instance this method can use to build pipeline state objects. - Returns:    A denoiser scaler instance upon success, or @nil@ otherwise.
--
-- ObjC selector: @- newTemporalDenoisedScalerWithDevice:compiler:@
newTemporalDenoisedScalerWithDevice_compiler :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> RawId -> RawId -> IO RawId
newTemporalDenoisedScalerWithDevice_compiler mtlfxTemporalDenoisedScalerDescriptor  device compiler =
    fmap (RawId . castPtr) $ sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "newTemporalDenoisedScalerWithDevice:compiler:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId compiler) :: Ptr ())]

-- | Returns the smallest temporal scaling factor the device supports as a floating-point value.
--
-- - Parameters:    - device: The Metal device for which this method checks the minimum input content scale it supports.
--
-- - Returns: the minimum input content scale the GPU device supports.
--
-- ObjC selector: @+ supportedInputContentMinScaleForDevice:@
supportedInputContentMinScaleForDevice :: RawId -> IO CFloat
supportedInputContentMinScaleForDevice device =
  do
    cls' <- getRequiredClass "MTLFXTemporalDenoisedScalerDescriptor"
    sendClassMsg cls' (mkSelector "supportedInputContentMinScaleForDevice:") retCFloat [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | Returns the largest temporal scaling factor the device supports as a floating-point value.
--
-- - Parameters:    - device: The Metal device for which this method checks the maximum input content scale it supports.
--
-- - Returns: the maximum input content scale the GPU device supports.
--
-- ObjC selector: @+ supportedInputContentMaxScaleForDevice:@
supportedInputContentMaxScaleForDevice :: RawId -> IO CFloat
supportedInputContentMaxScaleForDevice device =
  do
    cls' <- getRequiredClass "MTLFXTemporalDenoisedScalerDescriptor"
    sendClassMsg cls' (mkSelector "supportedInputContentMaxScaleForDevice:") retCFloat [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | Queries whether a Metal device supports denosing scaling compatible on Metal 4.
--
-- - Parameters:    - device: The GPU device for which this methods tests support.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports denoising scaling for             Metal 4, <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsMetal4FX:@
supportsMetal4FX :: RawId -> IO Bool
supportsMetal4FX device =
  do
    cls' <- getRequiredClass "MTLFXTemporalDenoisedScalerDescriptor"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsMetal4FX:") retCULong [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | Queries whether a Metal device supports denoising scaling.
--
-- - Parameters:    - device: The GPU device for which this methods tests support.
--
-- - Returns: <doc://com.apple.documentation/documentation/swift/true> if the device supports denoising scaling,             <doc://com.apple.documentation/documentation/swift/false> otherwise.
--
-- ObjC selector: @+ supportsDevice:@
supportsDevice :: RawId -> IO Bool
supportsDevice device =
  do
    cls' <- getRequiredClass "MTLFXTemporalDenoisedScalerDescriptor"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDevice:") retCULong [argPtr (castPtr (unRawId device) :: Ptr ())]

-- | The pixel format of the input color texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- colorTextureFormat@
colorTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
colorTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "colorTextureFormat") retCInt []

-- | The pixel format of the input color texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setColorTextureFormat:@
setColorTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setColorTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setColorTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input depth texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- depthTextureFormat@
depthTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
depthTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "depthTextureFormat") retCInt []

-- | The pixel format of the input depth texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setDepthTextureFormat:@
setDepthTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setDepthTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setDepthTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input motion texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- motionTextureFormat@
motionTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
motionTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "motionTextureFormat") retCInt []

-- | The pixel format of the input motion texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setMotionTextureFormat:@
setMotionTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setMotionTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setMotionTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input diffuse albedo texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- diffuseAlbedoTextureFormat@
diffuseAlbedoTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
diffuseAlbedoTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "diffuseAlbedoTextureFormat") retCInt []

-- | The pixel format of the input diffuse albedo texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setDiffuseAlbedoTextureFormat:@
setDiffuseAlbedoTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setDiffuseAlbedoTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setDiffuseAlbedoTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input specular albedo texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- specularAlbedoTextureFormat@
specularAlbedoTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
specularAlbedoTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "specularAlbedoTextureFormat") retCInt []

-- | The pixel format of the input specular albedo texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setSpecularAlbedoTextureFormat:@
setSpecularAlbedoTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setSpecularAlbedoTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setSpecularAlbedoTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input normal texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- normalTextureFormat@
normalTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
normalTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "normalTextureFormat") retCInt []

-- | The pixel format of the input normal texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setNormalTextureFormat:@
setNormalTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setNormalTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setNormalTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input roughness texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- roughnessTextureFormat@
roughnessTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
roughnessTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "roughnessTextureFormat") retCInt []

-- | The pixel format of the input roughness texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setRoughnessTextureFormat:@
setRoughnessTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setRoughnessTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setRoughnessTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input specular hit texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- specularHitDistanceTextureFormat@
specularHitDistanceTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
specularHitDistanceTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "specularHitDistanceTextureFormat") retCInt []

-- | The pixel format of the input specular hit texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setSpecularHitDistanceTextureFormat:@
setSpecularHitDistanceTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setSpecularHitDistanceTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setSpecularHitDistanceTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input denoise strength mask texture for the scaler you create with this descriptor.
--
-- You typically set this to a single-channel texture format.
--
-- ObjC selector: @- denoiseStrengthMaskTextureFormat@
denoiseStrengthMaskTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
denoiseStrengthMaskTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "denoiseStrengthMaskTextureFormat") retCInt []

-- | The pixel format of the input denoise strength mask texture for the scaler you create with this descriptor.
--
-- You typically set this to a single-channel texture format.
--
-- ObjC selector: @- setDenoiseStrengthMaskTextureFormat:@
setDenoiseStrengthMaskTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setDenoiseStrengthMaskTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setDenoiseStrengthMaskTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the input transparency overlay texture for the scaler you create with this descriptor.
--
-- You typically set this to a 4-channel RGBA texture format.
--
-- ObjC selector: @- transparencyOverlayTextureFormat@
transparencyOverlayTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
transparencyOverlayTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "transparencyOverlayTextureFormat") retCInt []

-- | The pixel format of the input transparency overlay texture for the scaler you create with this descriptor.
--
-- You typically set this to a 4-channel RGBA texture format.
--
-- ObjC selector: @- setTransparencyOverlayTextureFormat:@
setTransparencyOverlayTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setTransparencyOverlayTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setTransparencyOverlayTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The pixel format of the output color texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- outputTextureFormat@
outputTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
outputTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "outputTextureFormat") retCInt []

-- | The pixel format of the output color texture for the scaler you create with this descriptor.
--
-- ObjC selector: @- setOutputTextureFormat:@
setOutputTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setOutputTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setOutputTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | The width, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- inputWidth@
inputWidth :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CULong
inputWidth mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "inputWidth") retCULong []

-- | The width, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- setInputWidth:@
setInputWidth :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CULong -> IO ()
setInputWidth mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setInputWidth:") retVoid [argCULong value]

-- | The height, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- inputHeight@
inputHeight :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CULong
inputHeight mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "inputHeight") retCULong []

-- | The height, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- setInputHeight:@
setInputHeight :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CULong -> IO ()
setInputHeight mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setInputHeight:") retVoid [argCULong value]

-- | The width, in pixels, of the output color texture for the denoiser scaler.
--
-- ObjC selector: @- outputWidth@
outputWidth :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CULong
outputWidth mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "outputWidth") retCULong []

-- | The width, in pixels, of the output color texture for the denoiser scaler.
--
-- ObjC selector: @- setOutputWidth:@
setOutputWidth :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CULong -> IO ()
setOutputWidth mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setOutputWidth:") retVoid [argCULong value]

-- | The height, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- outputHeight@
outputHeight :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CULong
outputHeight mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "outputHeight") retCULong []

-- | The height, in pixels, of the input color texture for the denoiser scaler.
--
-- ObjC selector: @- setOutputHeight:@
setOutputHeight :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CULong -> IO ()
setOutputHeight mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setOutputHeight:") retVoid [argCULong value]

-- | A Boolean value that indicates whether MetalFX compiles a temporal scaling effect’s underlying upscaler as it creates the instance.
--
-- This property gives you the option to decide when it’s better for your app to give MetalFX the time it needs to compile the underlying upscaler of the temporal scaling effect. The two choices are:
--
-- * As you create the effect * After you create the effect, likely when your app needs to upscale the initial textures
--
-- You can create a temporal denoised scaler instance that can denoise and upscale textures at its best speed immediately after you create it by setting this property to <doc://com.apple.documentation/documentation/swift/true> and then calling an initialization method like ``newTemporalDenoisedScalerWithDevice:``. However, it may take MetalFX more time for that method to return while it creates the denoiser scaler and compiles its underlying pipelines.
--
-- By default, the property is equal to <doc://com.apple.documentation/documentation/swift/false>, which tells MetalFX to quickly create and return the temporal scaling-effect instance, and then compile a faster upscaler in the background. However, this means the effect can take more time to upscale textures while the framework compiles the underlying upscaler. When the framework finishes compiling, the effect runs just as fast as if you set the property to <doc://com.apple.documentation/documentation/swift/true>.
--
-- * Note: The image quality of the effect’s output texture is consistent, whether it’s using the slower interim upscaler or the final, faster upscaler.
--
-- ObjC selector: @- requiresSynchronousInitialization@
requiresSynchronousInitialization :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
requiresSynchronousInitialization mtlfxTemporalDenoisedScalerDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "requiresSynchronousInitialization") retCULong []

-- | A Boolean value that indicates whether MetalFX compiles a temporal scaling effect’s underlying upscaler as it creates the instance.
--
-- This property gives you the option to decide when it’s better for your app to give MetalFX the time it needs to compile the underlying upscaler of the temporal scaling effect. The two choices are:
--
-- * As you create the effect * After you create the effect, likely when your app needs to upscale the initial textures
--
-- You can create a temporal denoised scaler instance that can denoise and upscale textures at its best speed immediately after you create it by setting this property to <doc://com.apple.documentation/documentation/swift/true> and then calling an initialization method like ``newTemporalDenoisedScalerWithDevice:``. However, it may take MetalFX more time for that method to return while it creates the denoiser scaler and compiles its underlying pipelines.
--
-- By default, the property is equal to <doc://com.apple.documentation/documentation/swift/false>, which tells MetalFX to quickly create and return the temporal scaling-effect instance, and then compile a faster upscaler in the background. However, this means the effect can take more time to upscale textures while the framework compiles the underlying upscaler. When the framework finishes compiling, the effect runs just as fast as if you set the property to <doc://com.apple.documentation/documentation/swift/true>.
--
-- * Note: The image quality of the effect’s output texture is consistent, whether it’s using the slower interim upscaler or the final, faster upscaler.
--
-- ObjC selector: @- setRequiresSynchronousInitialization:@
setRequiresSynchronousInitialization :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setRequiresSynchronousInitialization mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setRequiresSynchronousInitialization:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether MetalFX calculates the exposure for each frame.
--
-- Set this property to <doc://com.apple.documentation/documentation/swift/true> to create a scaler that automatically calculates the exposure level for each image it scales.
--
-- * Note: Denoiser scaler instances that use auto exposure ignore their ``MTLFXTemporalScalerBase/exposureTexture`` property.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- autoExposureEnabled@
autoExposureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
autoExposureEnabled mtlfxTemporalDenoisedScalerDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "autoExposureEnabled") retCULong []

-- | A Boolean value that indicates whether MetalFX calculates the exposure for each frame.
--
-- Set this property to <doc://com.apple.documentation/documentation/swift/true> to create a scaler that automatically calculates the exposure level for each image it scales.
--
-- * Note: Denoiser scaler instances that use auto exposure ignore their ``MTLFXTemporalScalerBase/exposureTexture`` property.
--
-- This property's default value is <doc://com.apple.documentation/documentation/swift/false>.
--
-- ObjC selector: @- setAutoExposureEnabled:@
setAutoExposureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setAutoExposureEnabled mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setAutoExposureEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether a scaler you create from this descriptor applies a reactive mask.
--
-- ObjC selector: @- reactiveMaskTextureEnabled@
reactiveMaskTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
reactiveMaskTextureEnabled mtlfxTemporalDenoisedScalerDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "reactiveMaskTextureEnabled") retCULong []

-- | A Boolean value that indicates whether a scaler you create from this descriptor applies a reactive mask.
--
-- ObjC selector: @- setReactiveMaskTextureEnabled:@
setReactiveMaskTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setReactiveMaskTextureEnabled mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setReactiveMaskTextureEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | The pixel format of the reactive mask input texture for a scaler you create from this descriptor.
--
-- ObjC selector: @- reactiveMaskTextureFormat@
reactiveMaskTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO CInt
reactiveMaskTextureFormat mtlfxTemporalDenoisedScalerDescriptor  =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "reactiveMaskTextureFormat") retCInt []

-- | The pixel format of the reactive mask input texture for a scaler you create from this descriptor.
--
-- ObjC selector: @- setReactiveMaskTextureFormat:@
setReactiveMaskTextureFormat :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> CInt -> IO ()
setReactiveMaskTextureFormat mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setReactiveMaskTextureFormat:") retVoid [argCInt (fromIntegral value)]

-- | A Boolean value indicating whether the scaler evaluates a specular hit distance texture as part of its operation.
--
-- ObjC selector: @- specularHitDistanceTextureEnabled@
specularHitDistanceTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
specularHitDistanceTextureEnabled mtlfxTemporalDenoisedScalerDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "specularHitDistanceTextureEnabled") retCULong []

-- | A Boolean value indicating whether the scaler evaluates a specular hit distance texture as part of its operation.
--
-- ObjC selector: @- setSpecularHitDistanceTextureEnabled:@
setSpecularHitDistanceTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setSpecularHitDistanceTextureEnabled mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setSpecularHitDistanceTextureEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether the scaler evaluates a denoise strength mask texture as part of its operation.
--
-- ObjC selector: @- denoiseStrengthMaskTextureEnabled@
denoiseStrengthMaskTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
denoiseStrengthMaskTextureEnabled mtlfxTemporalDenoisedScalerDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "denoiseStrengthMaskTextureEnabled") retCULong []

-- | A Boolean value indicating whether the scaler evaluates a denoise strength mask texture as part of its operation.
--
-- ObjC selector: @- setDenoiseStrengthMaskTextureEnabled:@
setDenoiseStrengthMaskTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setDenoiseStrengthMaskTextureEnabled mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setDenoiseStrengthMaskTextureEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value indicating whether the scaler evaluates a transparency overlay texture as part of its operation.
--
-- ObjC selector: @- transparencyOverlayTextureEnabled@
transparencyOverlayTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> IO Bool
transparencyOverlayTextureEnabled mtlfxTemporalDenoisedScalerDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "transparencyOverlayTextureEnabled") retCULong []

-- | A Boolean value indicating whether the scaler evaluates a transparency overlay texture as part of its operation.
--
-- ObjC selector: @- setTransparencyOverlayTextureEnabled:@
setTransparencyOverlayTextureEnabled :: IsMTLFXTemporalDenoisedScalerDescriptor mtlfxTemporalDenoisedScalerDescriptor => mtlfxTemporalDenoisedScalerDescriptor -> Bool -> IO ()
setTransparencyOverlayTextureEnabled mtlfxTemporalDenoisedScalerDescriptor  value =
    sendMsg mtlfxTemporalDenoisedScalerDescriptor (mkSelector "setTransparencyOverlayTextureEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newTemporalDenoisedScalerWithDevice:@
newTemporalDenoisedScalerWithDeviceSelector :: Selector
newTemporalDenoisedScalerWithDeviceSelector = mkSelector "newTemporalDenoisedScalerWithDevice:"

-- | @Selector@ for @newTemporalDenoisedScalerWithDevice:compiler:@
newTemporalDenoisedScalerWithDevice_compilerSelector :: Selector
newTemporalDenoisedScalerWithDevice_compilerSelector = mkSelector "newTemporalDenoisedScalerWithDevice:compiler:"

-- | @Selector@ for @supportedInputContentMinScaleForDevice:@
supportedInputContentMinScaleForDeviceSelector :: Selector
supportedInputContentMinScaleForDeviceSelector = mkSelector "supportedInputContentMinScaleForDevice:"

-- | @Selector@ for @supportedInputContentMaxScaleForDevice:@
supportedInputContentMaxScaleForDeviceSelector :: Selector
supportedInputContentMaxScaleForDeviceSelector = mkSelector "supportedInputContentMaxScaleForDevice:"

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

-- | @Selector@ for @depthTextureFormat@
depthTextureFormatSelector :: Selector
depthTextureFormatSelector = mkSelector "depthTextureFormat"

-- | @Selector@ for @setDepthTextureFormat:@
setDepthTextureFormatSelector :: Selector
setDepthTextureFormatSelector = mkSelector "setDepthTextureFormat:"

-- | @Selector@ for @motionTextureFormat@
motionTextureFormatSelector :: Selector
motionTextureFormatSelector = mkSelector "motionTextureFormat"

-- | @Selector@ for @setMotionTextureFormat:@
setMotionTextureFormatSelector :: Selector
setMotionTextureFormatSelector = mkSelector "setMotionTextureFormat:"

-- | @Selector@ for @diffuseAlbedoTextureFormat@
diffuseAlbedoTextureFormatSelector :: Selector
diffuseAlbedoTextureFormatSelector = mkSelector "diffuseAlbedoTextureFormat"

-- | @Selector@ for @setDiffuseAlbedoTextureFormat:@
setDiffuseAlbedoTextureFormatSelector :: Selector
setDiffuseAlbedoTextureFormatSelector = mkSelector "setDiffuseAlbedoTextureFormat:"

-- | @Selector@ for @specularAlbedoTextureFormat@
specularAlbedoTextureFormatSelector :: Selector
specularAlbedoTextureFormatSelector = mkSelector "specularAlbedoTextureFormat"

-- | @Selector@ for @setSpecularAlbedoTextureFormat:@
setSpecularAlbedoTextureFormatSelector :: Selector
setSpecularAlbedoTextureFormatSelector = mkSelector "setSpecularAlbedoTextureFormat:"

-- | @Selector@ for @normalTextureFormat@
normalTextureFormatSelector :: Selector
normalTextureFormatSelector = mkSelector "normalTextureFormat"

-- | @Selector@ for @setNormalTextureFormat:@
setNormalTextureFormatSelector :: Selector
setNormalTextureFormatSelector = mkSelector "setNormalTextureFormat:"

-- | @Selector@ for @roughnessTextureFormat@
roughnessTextureFormatSelector :: Selector
roughnessTextureFormatSelector = mkSelector "roughnessTextureFormat"

-- | @Selector@ for @setRoughnessTextureFormat:@
setRoughnessTextureFormatSelector :: Selector
setRoughnessTextureFormatSelector = mkSelector "setRoughnessTextureFormat:"

-- | @Selector@ for @specularHitDistanceTextureFormat@
specularHitDistanceTextureFormatSelector :: Selector
specularHitDistanceTextureFormatSelector = mkSelector "specularHitDistanceTextureFormat"

-- | @Selector@ for @setSpecularHitDistanceTextureFormat:@
setSpecularHitDistanceTextureFormatSelector :: Selector
setSpecularHitDistanceTextureFormatSelector = mkSelector "setSpecularHitDistanceTextureFormat:"

-- | @Selector@ for @denoiseStrengthMaskTextureFormat@
denoiseStrengthMaskTextureFormatSelector :: Selector
denoiseStrengthMaskTextureFormatSelector = mkSelector "denoiseStrengthMaskTextureFormat"

-- | @Selector@ for @setDenoiseStrengthMaskTextureFormat:@
setDenoiseStrengthMaskTextureFormatSelector :: Selector
setDenoiseStrengthMaskTextureFormatSelector = mkSelector "setDenoiseStrengthMaskTextureFormat:"

-- | @Selector@ for @transparencyOverlayTextureFormat@
transparencyOverlayTextureFormatSelector :: Selector
transparencyOverlayTextureFormatSelector = mkSelector "transparencyOverlayTextureFormat"

-- | @Selector@ for @setTransparencyOverlayTextureFormat:@
setTransparencyOverlayTextureFormatSelector :: Selector
setTransparencyOverlayTextureFormatSelector = mkSelector "setTransparencyOverlayTextureFormat:"

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

-- | @Selector@ for @requiresSynchronousInitialization@
requiresSynchronousInitializationSelector :: Selector
requiresSynchronousInitializationSelector = mkSelector "requiresSynchronousInitialization"

-- | @Selector@ for @setRequiresSynchronousInitialization:@
setRequiresSynchronousInitializationSelector :: Selector
setRequiresSynchronousInitializationSelector = mkSelector "setRequiresSynchronousInitialization:"

-- | @Selector@ for @autoExposureEnabled@
autoExposureEnabledSelector :: Selector
autoExposureEnabledSelector = mkSelector "autoExposureEnabled"

-- | @Selector@ for @setAutoExposureEnabled:@
setAutoExposureEnabledSelector :: Selector
setAutoExposureEnabledSelector = mkSelector "setAutoExposureEnabled:"

-- | @Selector@ for @reactiveMaskTextureEnabled@
reactiveMaskTextureEnabledSelector :: Selector
reactiveMaskTextureEnabledSelector = mkSelector "reactiveMaskTextureEnabled"

-- | @Selector@ for @setReactiveMaskTextureEnabled:@
setReactiveMaskTextureEnabledSelector :: Selector
setReactiveMaskTextureEnabledSelector = mkSelector "setReactiveMaskTextureEnabled:"

-- | @Selector@ for @reactiveMaskTextureFormat@
reactiveMaskTextureFormatSelector :: Selector
reactiveMaskTextureFormatSelector = mkSelector "reactiveMaskTextureFormat"

-- | @Selector@ for @setReactiveMaskTextureFormat:@
setReactiveMaskTextureFormatSelector :: Selector
setReactiveMaskTextureFormatSelector = mkSelector "setReactiveMaskTextureFormat:"

-- | @Selector@ for @specularHitDistanceTextureEnabled@
specularHitDistanceTextureEnabledSelector :: Selector
specularHitDistanceTextureEnabledSelector = mkSelector "specularHitDistanceTextureEnabled"

-- | @Selector@ for @setSpecularHitDistanceTextureEnabled:@
setSpecularHitDistanceTextureEnabledSelector :: Selector
setSpecularHitDistanceTextureEnabledSelector = mkSelector "setSpecularHitDistanceTextureEnabled:"

-- | @Selector@ for @denoiseStrengthMaskTextureEnabled@
denoiseStrengthMaskTextureEnabledSelector :: Selector
denoiseStrengthMaskTextureEnabledSelector = mkSelector "denoiseStrengthMaskTextureEnabled"

-- | @Selector@ for @setDenoiseStrengthMaskTextureEnabled:@
setDenoiseStrengthMaskTextureEnabledSelector :: Selector
setDenoiseStrengthMaskTextureEnabledSelector = mkSelector "setDenoiseStrengthMaskTextureEnabled:"

-- | @Selector@ for @transparencyOverlayTextureEnabled@
transparencyOverlayTextureEnabledSelector :: Selector
transparencyOverlayTextureEnabledSelector = mkSelector "transparencyOverlayTextureEnabled"

-- | @Selector@ for @setTransparencyOverlayTextureEnabled:@
setTransparencyOverlayTextureEnabledSelector :: Selector
setTransparencyOverlayTextureEnabledSelector = mkSelector "setTransparencyOverlayTextureEnabled:"

