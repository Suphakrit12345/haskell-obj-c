{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams
  ( MTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams
  , IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams(..)
  , imageCodec
  , setImageCodec
  , maxFrameRate
  , setMaxFrameRate
  , minResolution
  , setMinResolution
  , maxResolution
  , setMaxResolution
  , quality
  , setQuality
  , watermarkEnabled
  , setWatermarkEnabled
  , osdEnabled
  , setOsdEnabled
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , imageCodecSelector
  , setImageCodecSelector
  , maxFrameRateSelector
  , setMaxFrameRateSelector
  , minResolutionSelector
  , setMinResolutionSelector
  , maxResolutionSelector
  , setMaxResolutionSelector
  , qualitySelector
  , setQualitySelector
  , watermarkEnabledSelector
  , setWatermarkEnabledSelector
  , osdEnabledSelector
  , setOsdEnabledSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- imageCodec@
imageCodec :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
imageCodec mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "imageCodec") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageCodec:@
setImageCodec :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setImageCodec mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "setImageCodec:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxFrameRate@
maxFrameRate :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
maxFrameRate mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "maxFrameRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxFrameRate:@
setMaxFrameRate :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setMaxFrameRate mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "setMaxFrameRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minResolution@
minResolution :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolution mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "minResolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinResolution:@
setMinResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setMinResolution mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "setMinResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxResolution@
maxResolution :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolution mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "maxResolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxResolution:@
setMaxResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setMaxResolution mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "setMaxResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- quality@
quality :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
quality mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "quality") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQuality:@
setQuality :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setQuality mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "setQuality:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "watermarkEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "setWatermarkEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "osdEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "setOsdEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageCodec@
imageCodecSelector :: Selector
imageCodecSelector = mkSelector "imageCodec"

-- | @Selector@ for @setImageCodec:@
setImageCodecSelector :: Selector
setImageCodecSelector = mkSelector "setImageCodec:"

-- | @Selector@ for @maxFrameRate@
maxFrameRateSelector :: Selector
maxFrameRateSelector = mkSelector "maxFrameRate"

-- | @Selector@ for @setMaxFrameRate:@
setMaxFrameRateSelector :: Selector
setMaxFrameRateSelector = mkSelector "setMaxFrameRate:"

-- | @Selector@ for @minResolution@
minResolutionSelector :: Selector
minResolutionSelector = mkSelector "minResolution"

-- | @Selector@ for @setMinResolution:@
setMinResolutionSelector :: Selector
setMinResolutionSelector = mkSelector "setMinResolution:"

-- | @Selector@ for @maxResolution@
maxResolutionSelector :: Selector
maxResolutionSelector = mkSelector "maxResolution"

-- | @Selector@ for @setMaxResolution:@
setMaxResolutionSelector :: Selector
setMaxResolutionSelector = mkSelector "setMaxResolution:"

-- | @Selector@ for @quality@
qualitySelector :: Selector
qualitySelector = mkSelector "quality"

-- | @Selector@ for @setQuality:@
setQualitySelector :: Selector
setQualitySelector = mkSelector "setQuality:"

-- | @Selector@ for @watermarkEnabled@
watermarkEnabledSelector :: Selector
watermarkEnabledSelector = mkSelector "watermarkEnabled"

-- | @Selector@ for @setWatermarkEnabled:@
setWatermarkEnabledSelector :: Selector
setWatermarkEnabledSelector = mkSelector "setWatermarkEnabled:"

-- | @Selector@ for @osdEnabled@
osdEnabledSelector :: Selector
osdEnabledSelector = mkSelector "osdEnabled"

-- | @Selector@ for @setOsdEnabled:@
setOsdEnabledSelector :: Selector
setOsdEnabledSelector = mkSelector "setOsdEnabled:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

