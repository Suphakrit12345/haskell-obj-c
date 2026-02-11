{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportConfigurationStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportConfigurationStruct
  ( MTRPushAVStreamTransportClusterTransportConfigurationStruct
  , IsMTRPushAVStreamTransportClusterTransportConfigurationStruct(..)
  , connectionID
  , setConnectionID
  , transportStatus
  , setTransportStatus
  , transportOptions
  , setTransportOptions
  , fabricIndex
  , setFabricIndex
  , connectionIDSelector
  , setConnectionIDSelector
  , transportStatusSelector
  , setTransportStatusSelector
  , transportOptionsSelector
  , setTransportOptionsSelector
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

-- | @- connectionID@
connectionID :: IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterTransportConfigurationStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportConfigurationStruct (mkSelector "connectionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterTransportConfigurationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportConfigurationStruct (mkSelector "setConnectionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transportStatus@
transportStatus :: IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> IO (Id NSNumber)
transportStatus mtrPushAVStreamTransportClusterTransportConfigurationStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportConfigurationStruct (mkSelector "transportStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransportStatus:@
setTransportStatus :: (IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> value -> IO ()
setTransportStatus mtrPushAVStreamTransportClusterTransportConfigurationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportConfigurationStruct (mkSelector "setTransportStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transportOptions@
transportOptions :: IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> IO (Id MTRPushAVStreamTransportClusterTransportOptionsStruct)
transportOptions mtrPushAVStreamTransportClusterTransportConfigurationStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportConfigurationStruct (mkSelector "transportOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransportOptions:@
setTransportOptions :: (IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct, IsMTRPushAVStreamTransportClusterTransportOptionsStruct value) => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> value -> IO ()
setTransportOptions mtrPushAVStreamTransportClusterTransportConfigurationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportConfigurationStruct (mkSelector "setTransportOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> IO (Id NSNumber)
fabricIndex mtrPushAVStreamTransportClusterTransportConfigurationStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportConfigurationStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> value -> IO ()
setFabricIndex mtrPushAVStreamTransportClusterTransportConfigurationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportConfigurationStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector
setConnectionIDSelector = mkSelector "setConnectionID:"

-- | @Selector@ for @transportStatus@
transportStatusSelector :: Selector
transportStatusSelector = mkSelector "transportStatus"

-- | @Selector@ for @setTransportStatus:@
setTransportStatusSelector :: Selector
setTransportStatusSelector = mkSelector "setTransportStatus:"

-- | @Selector@ for @transportOptions@
transportOptionsSelector :: Selector
transportOptionsSelector = mkSelector "transportOptions"

-- | @Selector@ for @setTransportOptions:@
setTransportOptionsSelector :: Selector
setTransportOptionsSelector = mkSelector "setTransportOptions:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

