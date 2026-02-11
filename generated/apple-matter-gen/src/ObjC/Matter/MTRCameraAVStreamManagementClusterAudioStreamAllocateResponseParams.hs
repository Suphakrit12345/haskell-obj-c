{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams
  ( MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams
  , IsMTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams(..)
  , initWithResponseValue_error
  , audioStreamID
  , setAudioStreamID
  , initWithResponseValue_errorSelector
  , audioStreamIDSelector
  , setAudioStreamIDSelector


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

-- | Initialize an MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams -> responseValue -> error_ -> IO (Id MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams)
initWithResponseValue_error mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- audioStreamID@
audioStreamID :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams -> IO (Id NSNumber)
audioStreamID mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams (mkSelector "audioStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams -> value -> IO ()
setAudioStreamID mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams (mkSelector "setAudioStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @audioStreamID@
audioStreamIDSelector :: Selector
audioStreamIDSelector = mkSelector "audioStreamID"

-- | @Selector@ for @setAudioStreamID:@
setAudioStreamIDSelector :: Selector
setAudioStreamIDSelector = mkSelector "setAudioStreamID:"

