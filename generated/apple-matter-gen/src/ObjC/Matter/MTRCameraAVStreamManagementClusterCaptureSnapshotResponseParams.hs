{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams
  ( MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams
  , IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams(..)
  , initWithResponseValue_error
  , data_
  , setData
  , imageCodec
  , setImageCodec
  , resolution
  , setResolution
  , initWithResponseValue_errorSelector
  , dataSelector
  , setDataSelector
  , imageCodecSelector
  , setImageCodecSelector
  , resolutionSelector
  , setResolutionSelector


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

-- | Initialize an MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> responseValue -> error_ -> IO (Id MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams)
initWithResponseValue_error mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- data@
data_ :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> IO (Id NSData)
data_ mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams  =
    sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams, IsNSData value) => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> value -> IO ()
setData mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageCodec@
imageCodec :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> IO (Id NSNumber)
imageCodec mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams  =
    sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams (mkSelector "imageCodec") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageCodec:@
setImageCodec :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> value -> IO ()
setImageCodec mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams (mkSelector "setImageCodec:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resolution@
resolution :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
resolution mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams  =
    sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams (mkSelector "resolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResolution:@
setResolution :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> value -> IO ()
setResolution mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams (mkSelector "setResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @imageCodec@
imageCodecSelector :: Selector
imageCodecSelector = mkSelector "imageCodec"

-- | @Selector@ for @setImageCodec:@
setImageCodecSelector :: Selector
setImageCodecSelector = mkSelector "setImageCodec:"

-- | @Selector@ for @resolution@
resolutionSelector :: Selector
resolutionSelector = mkSelector "resolution"

-- | @Selector@ for @setResolution:@
setResolutionSelector :: Selector
setResolutionSelector = mkSelector "setResolution:"

