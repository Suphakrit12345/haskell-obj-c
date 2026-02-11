{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeWebRTCSessionStruct@.
module ObjC.Matter.MTRDataTypeWebRTCSessionStruct
  ( MTRDataTypeWebRTCSessionStruct
  , IsMTRDataTypeWebRTCSessionStruct(..)
  , id_
  , setId
  , peerNodeID
  , setPeerNodeID
  , peerEndpointID
  , setPeerEndpointID
  , streamUsage
  , setStreamUsage
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , metadataEnabled
  , setMetadataEnabled
  , fabricIndex
  , setFabricIndex
  , idSelector
  , setIdSelector
  , peerNodeIDSelector
  , setPeerNodeIDSelector
  , peerEndpointIDSelector
  , setPeerEndpointIDSelector
  , streamUsageSelector
  , setStreamUsageSelector
  , videoStreamIDSelector
  , setVideoStreamIDSelector
  , audioStreamIDSelector
  , setAudioStreamIDSelector
  , metadataEnabledSelector
  , setMetadataEnabledSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- id@
id_ :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
id_ mtrDataTypeWebRTCSessionStruct  =
    sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "id") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setId:@
setId :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setId mtrDataTypeWebRTCSessionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "setId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- peerNodeID@
peerNodeID :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
peerNodeID mtrDataTypeWebRTCSessionStruct  =
    sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "peerNodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPeerNodeID:@
setPeerNodeID :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setPeerNodeID mtrDataTypeWebRTCSessionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "setPeerNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- peerEndpointID@
peerEndpointID :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
peerEndpointID mtrDataTypeWebRTCSessionStruct  =
    sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "peerEndpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPeerEndpointID:@
setPeerEndpointID :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setPeerEndpointID mtrDataTypeWebRTCSessionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "setPeerEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- streamUsage@
streamUsage :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
streamUsage mtrDataTypeWebRTCSessionStruct  =
    sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "streamUsage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setStreamUsage mtrDataTypeWebRTCSessionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "setStreamUsage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- videoStreamID@
videoStreamID :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
videoStreamID mtrDataTypeWebRTCSessionStruct  =
    sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setVideoStreamID mtrDataTypeWebRTCSessionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioStreamID@
audioStreamID :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
audioStreamID mtrDataTypeWebRTCSessionStruct  =
    sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "audioStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setAudioStreamID mtrDataTypeWebRTCSessionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "setAudioStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metadataEnabled@
metadataEnabled :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
metadataEnabled mtrDataTypeWebRTCSessionStruct  =
    sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "metadataEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetadataEnabled:@
setMetadataEnabled :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setMetadataEnabled mtrDataTypeWebRTCSessionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "setMetadataEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
fabricIndex mtrDataTypeWebRTCSessionStruct  =
    sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setFabricIndex mtrDataTypeWebRTCSessionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeWebRTCSessionStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @id@
idSelector :: Selector
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector
setIdSelector = mkSelector "setId:"

-- | @Selector@ for @peerNodeID@
peerNodeIDSelector :: Selector
peerNodeIDSelector = mkSelector "peerNodeID"

-- | @Selector@ for @setPeerNodeID:@
setPeerNodeIDSelector :: Selector
setPeerNodeIDSelector = mkSelector "setPeerNodeID:"

-- | @Selector@ for @peerEndpointID@
peerEndpointIDSelector :: Selector
peerEndpointIDSelector = mkSelector "peerEndpointID"

-- | @Selector@ for @setPeerEndpointID:@
setPeerEndpointIDSelector :: Selector
setPeerEndpointIDSelector = mkSelector "setPeerEndpointID:"

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector
setStreamUsageSelector = mkSelector "setStreamUsage:"

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @audioStreamID@
audioStreamIDSelector :: Selector
audioStreamIDSelector = mkSelector "audioStreamID"

-- | @Selector@ for @setAudioStreamID:@
setAudioStreamIDSelector :: Selector
setAudioStreamIDSelector = mkSelector "setAudioStreamID:"

-- | @Selector@ for @metadataEnabled@
metadataEnabledSelector :: Selector
metadataEnabledSelector = mkSelector "metadataEnabled"

-- | @Selector@ for @setMetadataEnabled:@
setMetadataEnabledSelector :: Selector
setMetadataEnabledSelector = mkSelector "setMetadataEnabled:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

