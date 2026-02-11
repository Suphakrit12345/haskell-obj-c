{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams
  ( MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams
  , IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams(..)
  , initWithResponseValue_error
  , snapshotStreamID
  , setSnapshotStreamID
  , initWithResponseValue_errorSelector
  , snapshotStreamIDSelector
  , setSnapshotStreamIDSelector


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

-- | Initialize an MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams -> responseValue -> error_ -> IO (Id MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams)
initWithResponseValue_error mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- snapshotStreamID@
snapshotStreamID :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams -> IO (Id NSNumber)
snapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams (mkSelector "snapshotStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSnapshotStreamID:@
setSnapshotStreamID :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams -> value -> IO ()
setSnapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams (mkSelector "setSnapshotStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @snapshotStreamID@
snapshotStreamIDSelector :: Selector
snapshotStreamIDSelector = mkSelector "snapshotStreamID"

-- | @Selector@ for @setSnapshotStreamID:@
setSnapshotStreamIDSelector :: Selector
setSnapshotStreamIDSelector = mkSelector "setSnapshotStreamID:"

