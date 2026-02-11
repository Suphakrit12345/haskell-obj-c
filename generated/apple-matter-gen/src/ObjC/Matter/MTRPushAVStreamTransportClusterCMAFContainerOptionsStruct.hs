{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct
  ( MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct
  , IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct(..)
  , cmafInterface
  , setCmafInterface
  , segmentDuration
  , setSegmentDuration
  , chunkDuration
  , setChunkDuration
  , sessionGroup
  , setSessionGroup
  , trackName
  , setTrackName
  , cencKey
  , setCencKey
  , cencKeyID
  , setCencKeyID
  , metadataEnabled
  , setMetadataEnabled
  , cmafInterfaceSelector
  , setCmafInterfaceSelector
  , segmentDurationSelector
  , setSegmentDurationSelector
  , chunkDurationSelector
  , setChunkDurationSelector
  , sessionGroupSelector
  , setSessionGroupSelector
  , trackNameSelector
  , setTrackNameSelector
  , cencKeySelector
  , setCencKeySelector
  , cencKeyIDSelector
  , setCencKeyIDSelector
  , metadataEnabledSelector
  , setMetadataEnabledSelector


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

-- | @- cmafInterface@
cmafInterface :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
cmafInterface mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "cmafInterface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCmafInterface:@
setCmafInterface :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setCmafInterface mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "setCmafInterface:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- segmentDuration@
segmentDuration :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
segmentDuration mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "segmentDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSegmentDuration:@
setSegmentDuration :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setSegmentDuration mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "setSegmentDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- chunkDuration@
chunkDuration :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
chunkDuration mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "chunkDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChunkDuration:@
setChunkDuration :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setChunkDuration mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "setChunkDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sessionGroup@
sessionGroup :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
sessionGroup mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "sessionGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionGroup:@
setSessionGroup :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setSessionGroup mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "setSessionGroup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- trackName@
trackName :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSString)
trackName mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "trackName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTrackName:@
setTrackName :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSString value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setTrackName mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "setTrackName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cencKey@
cencKey :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSData)
cencKey mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "cencKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCencKey:@
setCencKey :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSData value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setCencKey mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "setCencKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cencKeyID@
cencKeyID :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSData)
cencKeyID mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "cencKeyID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCencKeyID:@
setCencKeyID :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSData value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setCencKeyID mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "setCencKeyID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metadataEnabled@
metadataEnabled :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
metadataEnabled mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "metadataEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetadataEnabled:@
setMetadataEnabled :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setMetadataEnabled mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct (mkSelector "setMetadataEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cmafInterface@
cmafInterfaceSelector :: Selector
cmafInterfaceSelector = mkSelector "cmafInterface"

-- | @Selector@ for @setCmafInterface:@
setCmafInterfaceSelector :: Selector
setCmafInterfaceSelector = mkSelector "setCmafInterface:"

-- | @Selector@ for @segmentDuration@
segmentDurationSelector :: Selector
segmentDurationSelector = mkSelector "segmentDuration"

-- | @Selector@ for @setSegmentDuration:@
setSegmentDurationSelector :: Selector
setSegmentDurationSelector = mkSelector "setSegmentDuration:"

-- | @Selector@ for @chunkDuration@
chunkDurationSelector :: Selector
chunkDurationSelector = mkSelector "chunkDuration"

-- | @Selector@ for @setChunkDuration:@
setChunkDurationSelector :: Selector
setChunkDurationSelector = mkSelector "setChunkDuration:"

-- | @Selector@ for @sessionGroup@
sessionGroupSelector :: Selector
sessionGroupSelector = mkSelector "sessionGroup"

-- | @Selector@ for @setSessionGroup:@
setSessionGroupSelector :: Selector
setSessionGroupSelector = mkSelector "setSessionGroup:"

-- | @Selector@ for @trackName@
trackNameSelector :: Selector
trackNameSelector = mkSelector "trackName"

-- | @Selector@ for @setTrackName:@
setTrackNameSelector :: Selector
setTrackNameSelector = mkSelector "setTrackName:"

-- | @Selector@ for @cencKey@
cencKeySelector :: Selector
cencKeySelector = mkSelector "cencKey"

-- | @Selector@ for @setCencKey:@
setCencKeySelector :: Selector
setCencKeySelector = mkSelector "setCencKey:"

-- | @Selector@ for @cencKeyID@
cencKeyIDSelector :: Selector
cencKeyIDSelector = mkSelector "cencKeyID"

-- | @Selector@ for @setCencKeyID:@
setCencKeyIDSelector :: Selector
setCencKeyIDSelector = mkSelector "setCencKeyID:"

-- | @Selector@ for @metadataEnabled@
metadataEnabledSelector :: Selector
metadataEnabledSelector = mkSelector "metadataEnabled"

-- | @Selector@ for @setMetadataEnabled:@
setMetadataEnabledSelector :: Selector
setMetadataEnabledSelector = mkSelector "setMetadataEnabled:"

