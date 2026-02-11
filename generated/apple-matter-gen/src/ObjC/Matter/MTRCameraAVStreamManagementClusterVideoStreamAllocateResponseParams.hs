{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams
  ( MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams
  , IsMTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams(..)
  , initWithResponseValue_error
  , videoStreamID
  , setVideoStreamID
  , initWithResponseValue_errorSelector
  , videoStreamIDSelector
  , setVideoStreamIDSelector


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

-- | Initialize an MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams -> responseValue -> error_ -> IO (Id MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams)
initWithResponseValue_error mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- videoStreamID@
videoStreamID :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams -> IO (Id NSNumber)
videoStreamID mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams -> value -> IO ()
setVideoStreamID mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

