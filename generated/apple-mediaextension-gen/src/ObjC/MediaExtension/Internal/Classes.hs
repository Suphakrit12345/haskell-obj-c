{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MediaExtension.Internal.Classes (
    module ObjC.MediaExtension.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MEByteSource ----------

-- | MEByteSource
--
-- Provides read access to the data in a media asset file.
--
-- The Media Toolbox passes an MEByteSource instance for the media asset's primary file when initializing an MEFormatReader object. The MEFormatReader may request additional MEByteSources be created for related files in the same directory as the primary file by calling the byteSourceForRelatedFileName method.
-- 
-- Phantom type for @MEByteSource@.
data MEByteSource

instance IsObjCObject (Id MEByteSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEByteSource"

class IsNSObject a => IsMEByteSource a where
  toMEByteSource :: a -> Id MEByteSource

instance IsMEByteSource (Id MEByteSource) where
  toMEByteSource = unsafeCastId

instance IsNSObject (Id MEByteSource) where
  toNSObject = unsafeCastId

-- ---------- MEDecodeFrameOptions ----------

-- | MEDecodeFrameOptions
--
-- Conveys directives or options from the VideoToolbox to guide decoder operation on a per-frame basis.
-- 
-- Phantom type for @MEDecodeFrameOptions@.
data MEDecodeFrameOptions

instance IsObjCObject (Id MEDecodeFrameOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEDecodeFrameOptions"

class IsNSObject a => IsMEDecodeFrameOptions a where
  toMEDecodeFrameOptions :: a -> Id MEDecodeFrameOptions

instance IsMEDecodeFrameOptions (Id MEDecodeFrameOptions) where
  toMEDecodeFrameOptions = unsafeCastId

instance IsNSObject (Id MEDecodeFrameOptions) where
  toNSObject = unsafeCastId

-- ---------- MEEstimatedSampleLocation ----------

-- | MEEstimatedSampleLocation
--
-- Provides information about the estimated sample location with the media.
--
-- An instance of this class is returned by calls to the MESampleCursor method estimatedSampleLocationReturningError.
-- 
-- Phantom type for @MEEstimatedSampleLocation@.
data MEEstimatedSampleLocation

instance IsObjCObject (Id MEEstimatedSampleLocation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEEstimatedSampleLocation"

class IsNSObject a => IsMEEstimatedSampleLocation a where
  toMEEstimatedSampleLocation :: a -> Id MEEstimatedSampleLocation

instance IsMEEstimatedSampleLocation (Id MEEstimatedSampleLocation) where
  toMEEstimatedSampleLocation = unsafeCastId

instance IsNSObject (Id MEEstimatedSampleLocation) where
  toNSObject = unsafeCastId

-- ---------- MEFileInfo ----------

-- | MEFileInfo
--
-- A class incorporating file properties parsed from the media asset.
--
-- The MEFileInfo properties are parsed asynchronously through the loadFileInfoWithCompletionHandler method of MEFormatReader.
-- 
-- Phantom type for @MEFileInfo@.
data MEFileInfo

instance IsObjCObject (Id MEFileInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEFileInfo"

class IsNSObject a => IsMEFileInfo a where
  toMEFileInfo :: a -> Id MEFileInfo

instance IsMEFileInfo (Id MEFileInfo) where
  toMEFileInfo = unsafeCastId

instance IsNSObject (Id MEFileInfo) where
  toNSObject = unsafeCastId

-- ---------- MEFormatReaderInstantiationOptions ----------

-- | MEFormatReaderInstantiationOptions
--
-- A class that encapsulates options to be passed to MEFormatReaderExtension
--
-- The class MEFormatReaderInstantiationOptions is mutable, with options set through instance properties.
-- 
-- Phantom type for @MEFormatReaderInstantiationOptions@.
data MEFormatReaderInstantiationOptions

instance IsObjCObject (Id MEFormatReaderInstantiationOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEFormatReaderInstantiationOptions"

class IsNSObject a => IsMEFormatReaderInstantiationOptions a where
  toMEFormatReaderInstantiationOptions :: a -> Id MEFormatReaderInstantiationOptions

instance IsMEFormatReaderInstantiationOptions (Id MEFormatReaderInstantiationOptions) where
  toMEFormatReaderInstantiationOptions = unsafeCastId

instance IsNSObject (Id MEFormatReaderInstantiationOptions) where
  toNSObject = unsafeCastId

-- ---------- MEHEVCDependencyInfo ----------

-- | MEHEVCDependencyInfo
--
-- Provides information about the HEVC dependency attributes of a sample.
--
-- An instance of this class is returned by MESampleCursor property hevcDependencyInfo.
-- 
-- Phantom type for @MEHEVCDependencyInfo@.
data MEHEVCDependencyInfo

instance IsObjCObject (Id MEHEVCDependencyInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEHEVCDependencyInfo"

class IsNSObject a => IsMEHEVCDependencyInfo a where
  toMEHEVCDependencyInfo :: a -> Id MEHEVCDependencyInfo

instance IsMEHEVCDependencyInfo (Id MEHEVCDependencyInfo) where
  toMEHEVCDependencyInfo = unsafeCastId

instance IsNSObject (Id MEHEVCDependencyInfo) where
  toNSObject = unsafeCastId

-- ---------- MERAWProcessingParameter ----------

-- | MERAWProcessingParameter
--
-- An object implementing this protocol is implemented by the RAW Processor to describe each processing parameter the Processor exposes.
--
-- The MERAWProcessingParameter protocol provides an interface for the VideoToolbox to query descriptions of the different parameters that can be used to influence Processor operation.  A distinct MERAWProcessingParameter is created for each parameter supported by the Processor, and the set of supported parameters is returned by the MERAWProcessor's processingParameters interface.
-- 
-- Phantom type for @MERAWProcessingParameter@.
data MERAWProcessingParameter

instance IsObjCObject (Id MERAWProcessingParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MERAWProcessingParameter"

class IsNSObject a => IsMERAWProcessingParameter a where
  toMERAWProcessingParameter :: a -> Id MERAWProcessingParameter

instance IsMERAWProcessingParameter (Id MERAWProcessingParameter) where
  toMERAWProcessingParameter = unsafeCastId

instance IsNSObject (Id MERAWProcessingParameter) where
  toNSObject = unsafeCastId

-- ---------- MERAWProcessorPixelBufferManager ----------

-- | MERAWProcessorPixelBufferManager
--
-- Describes pixel buffer requirements and creates new pixel buffers.
--
-- Contains the interfaces that the App Extension RAW processor uses for two tasks. First, to declare its set of requirements for output CVPixelBuffers in the form of a pixelBufferAttributes dictionary. Second, to create pixelBuffers which match processor output requirements but also satisfy VideoToolbox and client requirements.
-- 
-- Phantom type for @MERAWProcessorPixelBufferManager@.
data MERAWProcessorPixelBufferManager

instance IsObjCObject (Id MERAWProcessorPixelBufferManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MERAWProcessorPixelBufferManager"

class IsNSObject a => IsMERAWProcessorPixelBufferManager a where
  toMERAWProcessorPixelBufferManager :: a -> Id MERAWProcessorPixelBufferManager

instance IsMERAWProcessorPixelBufferManager (Id MERAWProcessorPixelBufferManager) where
  toMERAWProcessorPixelBufferManager = unsafeCastId

instance IsNSObject (Id MERAWProcessorPixelBufferManager) where
  toNSObject = unsafeCastId

-- ---------- MESampleCursorChunk ----------

-- | MESampleCursorChunk
--
-- Provides information about the chunk of media where a sample is located.
--
-- An instance of this class is returned by calls to the MESampleCursor method chunkDetails.
-- 
-- Phantom type for @MESampleCursorChunk@.
data MESampleCursorChunk

instance IsObjCObject (Id MESampleCursorChunk) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MESampleCursorChunk"

class IsNSObject a => IsMESampleCursorChunk a where
  toMESampleCursorChunk :: a -> Id MESampleCursorChunk

instance IsMESampleCursorChunk (Id MESampleCursorChunk) where
  toMESampleCursorChunk = unsafeCastId

instance IsNSObject (Id MESampleCursorChunk) where
  toNSObject = unsafeCastId

-- ---------- MESampleLocation ----------

-- | MESampleLocation
--
-- Provides information about the sample location with the media.
--
-- An instance of this class is returned by calls to the MESampleCursor method sampleLocation.
-- 
-- Phantom type for @MESampleLocation@.
data MESampleLocation

instance IsObjCObject (Id MESampleLocation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MESampleLocation"

class IsNSObject a => IsMESampleLocation a where
  toMESampleLocation :: a -> Id MESampleLocation

instance IsMESampleLocation (Id MESampleLocation) where
  toMESampleLocation = unsafeCastId

instance IsNSObject (Id MESampleLocation) where
  toNSObject = unsafeCastId

-- ---------- METrackInfo ----------

-- | METrackInfo
--
-- A class incorporating track properties parsed from the media asset.
--
-- The METrackInfo properties are parsed asynchronously through the loadTrackInfoWithCompletionHandler method of METrackReader.
-- 
-- Phantom type for @METrackInfo@.
data METrackInfo

instance IsObjCObject (Id METrackInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "METrackInfo"

class IsNSObject a => IsMETrackInfo a where
  toMETrackInfo :: a -> Id METrackInfo

instance IsMETrackInfo (Id METrackInfo) where
  toMETrackInfo = unsafeCastId

instance IsNSObject (Id METrackInfo) where
  toNSObject = unsafeCastId

-- ---------- MEVideoDecoderPixelBufferManager ----------

-- | MEVideoDecoderPixelBufferManager
--
-- Describes pixel buffer requirements and creates new pixel buffers.
--
-- Contains the interfaces that the App Extension video decoder uses for two tasks. First, to declare its set of requirements for output CVPixelBuffers in the form of a pixelBufferAttributes dictionary. Second, to create pixelBuffers which match decoder output requirements but also satisfy VideoToolbox and client requirements.
-- 
-- Phantom type for @MEVideoDecoderPixelBufferManager@.
data MEVideoDecoderPixelBufferManager

instance IsObjCObject (Id MEVideoDecoderPixelBufferManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEVideoDecoderPixelBufferManager"

class IsNSObject a => IsMEVideoDecoderPixelBufferManager a where
  toMEVideoDecoderPixelBufferManager :: a -> Id MEVideoDecoderPixelBufferManager

instance IsMEVideoDecoderPixelBufferManager (Id MEVideoDecoderPixelBufferManager) where
  toMEVideoDecoderPixelBufferManager = unsafeCastId

instance IsNSObject (Id MEVideoDecoderPixelBufferManager) where
  toNSObject = unsafeCastId

-- ---------- MERAWProcessingBooleanParameter ----------

-- | Phantom type for @MERAWProcessingBooleanParameter@.
data MERAWProcessingBooleanParameter

instance IsObjCObject (Id MERAWProcessingBooleanParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MERAWProcessingBooleanParameter"

class IsMERAWProcessingParameter a => IsMERAWProcessingBooleanParameter a where
  toMERAWProcessingBooleanParameter :: a -> Id MERAWProcessingBooleanParameter

instance IsMERAWProcessingBooleanParameter (Id MERAWProcessingBooleanParameter) where
  toMERAWProcessingBooleanParameter = unsafeCastId

instance IsMERAWProcessingParameter (Id MERAWProcessingBooleanParameter) where
  toMERAWProcessingParameter = unsafeCastId

instance IsNSObject (Id MERAWProcessingBooleanParameter) where
  toNSObject = unsafeCastId

-- ---------- MERAWProcessingFloatParameter ----------

-- | Phantom type for @MERAWProcessingFloatParameter@.
data MERAWProcessingFloatParameter

instance IsObjCObject (Id MERAWProcessingFloatParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MERAWProcessingFloatParameter"

class IsMERAWProcessingParameter a => IsMERAWProcessingFloatParameter a where
  toMERAWProcessingFloatParameter :: a -> Id MERAWProcessingFloatParameter

instance IsMERAWProcessingFloatParameter (Id MERAWProcessingFloatParameter) where
  toMERAWProcessingFloatParameter = unsafeCastId

instance IsMERAWProcessingParameter (Id MERAWProcessingFloatParameter) where
  toMERAWProcessingParameter = unsafeCastId

instance IsNSObject (Id MERAWProcessingFloatParameter) where
  toNSObject = unsafeCastId

-- ---------- MERAWProcessingIntegerParameter ----------

-- | Phantom type for @MERAWProcessingIntegerParameter@.
data MERAWProcessingIntegerParameter

instance IsObjCObject (Id MERAWProcessingIntegerParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MERAWProcessingIntegerParameter"

class IsMERAWProcessingParameter a => IsMERAWProcessingIntegerParameter a where
  toMERAWProcessingIntegerParameter :: a -> Id MERAWProcessingIntegerParameter

instance IsMERAWProcessingIntegerParameter (Id MERAWProcessingIntegerParameter) where
  toMERAWProcessingIntegerParameter = unsafeCastId

instance IsMERAWProcessingParameter (Id MERAWProcessingIntegerParameter) where
  toMERAWProcessingParameter = unsafeCastId

instance IsNSObject (Id MERAWProcessingIntegerParameter) where
  toNSObject = unsafeCastId

-- ---------- MERAWProcessingListElementParameter ----------

-- | MERAWProcessingListElementParameter
--
-- An object implementing this protocol is implemented by the RAW Processor to describe each processing parameter the Processor exposes.
--
-- The MERAWProcessingListElementParameter protocol provides an interface for VideoToolbox to query descriptions of the different elements in a parameter list  for a List element in a MERAWProcessingParameter.  A distinct MERAWProcessingListElementParameter is created for each list element.
-- 
-- Phantom type for @MERAWProcessingListElementParameter@.
data MERAWProcessingListElementParameter

instance IsObjCObject (Id MERAWProcessingListElementParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MERAWProcessingListElementParameter"

class IsMERAWProcessingParameter a => IsMERAWProcessingListElementParameter a where
  toMERAWProcessingListElementParameter :: a -> Id MERAWProcessingListElementParameter

instance IsMERAWProcessingListElementParameter (Id MERAWProcessingListElementParameter) where
  toMERAWProcessingListElementParameter = unsafeCastId

instance IsMERAWProcessingParameter (Id MERAWProcessingListElementParameter) where
  toMERAWProcessingParameter = unsafeCastId

instance IsNSObject (Id MERAWProcessingListElementParameter) where
  toNSObject = unsafeCastId

-- ---------- MERAWProcessingListParameter ----------

-- | Phantom type for @MERAWProcessingListParameter@.
data MERAWProcessingListParameter

instance IsObjCObject (Id MERAWProcessingListParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MERAWProcessingListParameter"

class IsMERAWProcessingParameter a => IsMERAWProcessingListParameter a where
  toMERAWProcessingListParameter :: a -> Id MERAWProcessingListParameter

instance IsMERAWProcessingListParameter (Id MERAWProcessingListParameter) where
  toMERAWProcessingListParameter = unsafeCastId

instance IsMERAWProcessingParameter (Id MERAWProcessingListParameter) where
  toMERAWProcessingParameter = unsafeCastId

instance IsNSObject (Id MERAWProcessingListParameter) where
  toNSObject = unsafeCastId

-- ---------- MERAWProcessingSubGroupParameter ----------

-- | Phantom type for @MERAWProcessingSubGroupParameter@.
data MERAWProcessingSubGroupParameter

instance IsObjCObject (Id MERAWProcessingSubGroupParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MERAWProcessingSubGroupParameter"

class IsMERAWProcessingParameter a => IsMERAWProcessingSubGroupParameter a where
  toMERAWProcessingSubGroupParameter :: a -> Id MERAWProcessingSubGroupParameter

instance IsMERAWProcessingSubGroupParameter (Id MERAWProcessingSubGroupParameter) where
  toMERAWProcessingSubGroupParameter = unsafeCastId

instance IsMERAWProcessingParameter (Id MERAWProcessingSubGroupParameter) where
  toMERAWProcessingParameter = unsafeCastId

instance IsNSObject (Id MERAWProcessingSubGroupParameter) where
  toNSObject = unsafeCastId
