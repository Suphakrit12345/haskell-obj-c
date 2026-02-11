{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDirectoryClusterThreadNetworkStruct@.
module ObjC.Matter.MTRThreadNetworkDirectoryClusterThreadNetworkStruct
  ( MTRThreadNetworkDirectoryClusterThreadNetworkStruct
  , IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct(..)
  , extendedPanID
  , setExtendedPanID
  , networkName
  , setNetworkName
  , channel
  , setChannel
  , activeTimestamp
  , setActiveTimestamp
  , extendedPanIDSelector
  , setExtendedPanIDSelector
  , networkNameSelector
  , setNetworkNameSelector
  , channelSelector
  , setChannelSelector
  , activeTimestampSelector
  , setActiveTimestampSelector


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

-- | @- extendedPanID@
extendedPanID :: IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> IO (Id NSData)
extendedPanID mtrThreadNetworkDirectoryClusterThreadNetworkStruct  =
    sendMsg mtrThreadNetworkDirectoryClusterThreadNetworkStruct (mkSelector "extendedPanID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtendedPanID:@
setExtendedPanID :: (IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct, IsNSData value) => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> value -> IO ()
setExtendedPanID mtrThreadNetworkDirectoryClusterThreadNetworkStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDirectoryClusterThreadNetworkStruct (mkSelector "setExtendedPanID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- networkName@
networkName :: IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> IO (Id NSString)
networkName mtrThreadNetworkDirectoryClusterThreadNetworkStruct  =
    sendMsg mtrThreadNetworkDirectoryClusterThreadNetworkStruct (mkSelector "networkName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkName:@
setNetworkName :: (IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct, IsNSString value) => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> value -> IO ()
setNetworkName mtrThreadNetworkDirectoryClusterThreadNetworkStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDirectoryClusterThreadNetworkStruct (mkSelector "setNetworkName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channel@
channel :: IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> IO (Id NSNumber)
channel mtrThreadNetworkDirectoryClusterThreadNetworkStruct  =
    sendMsg mtrThreadNetworkDirectoryClusterThreadNetworkStruct (mkSelector "channel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannel:@
setChannel :: (IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct, IsNSNumber value) => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> value -> IO ()
setChannel mtrThreadNetworkDirectoryClusterThreadNetworkStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDirectoryClusterThreadNetworkStruct (mkSelector "setChannel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- activeTimestamp@
activeTimestamp :: IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> IO (Id NSNumber)
activeTimestamp mtrThreadNetworkDirectoryClusterThreadNetworkStruct  =
    sendMsg mtrThreadNetworkDirectoryClusterThreadNetworkStruct (mkSelector "activeTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActiveTimestamp:@
setActiveTimestamp :: (IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct, IsNSNumber value) => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> value -> IO ()
setActiveTimestamp mtrThreadNetworkDirectoryClusterThreadNetworkStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDirectoryClusterThreadNetworkStruct (mkSelector "setActiveTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @extendedPanID@
extendedPanIDSelector :: Selector
extendedPanIDSelector = mkSelector "extendedPanID"

-- | @Selector@ for @setExtendedPanID:@
setExtendedPanIDSelector :: Selector
setExtendedPanIDSelector = mkSelector "setExtendedPanID:"

-- | @Selector@ for @networkName@
networkNameSelector :: Selector
networkNameSelector = mkSelector "networkName"

-- | @Selector@ for @setNetworkName:@
setNetworkNameSelector :: Selector
setNetworkNameSelector = mkSelector "setNetworkName:"

-- | @Selector@ for @channel@
channelSelector :: Selector
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @activeTimestamp@
activeTimestampSelector :: Selector
activeTimestampSelector = mkSelector "activeTimestamp"

-- | @Selector@ for @setActiveTimestamp:@
setActiveTimestampSelector :: Selector
setActiveTimestampSelector = mkSelector "setActiveTimestamp:"

