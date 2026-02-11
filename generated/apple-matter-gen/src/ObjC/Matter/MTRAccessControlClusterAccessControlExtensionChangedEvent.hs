{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlExtensionChangedEvent@.
module ObjC.Matter.MTRAccessControlClusterAccessControlExtensionChangedEvent
  ( MTRAccessControlClusterAccessControlExtensionChangedEvent
  , IsMTRAccessControlClusterAccessControlExtensionChangedEvent(..)
  , adminNodeID
  , setAdminNodeID
  , adminPasscodeID
  , setAdminPasscodeID
  , changeType
  , setChangeType
  , latestValue
  , setLatestValue
  , fabricIndex
  , setFabricIndex
  , adminNodeIDSelector
  , setAdminNodeIDSelector
  , adminPasscodeIDSelector
  , setAdminPasscodeIDSelector
  , changeTypeSelector
  , setChangeTypeSelector
  , latestValueSelector
  , setLatestValueSelector
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

-- | @- adminNodeID@
adminNodeID :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id NSNumber)
adminNodeID mtrAccessControlClusterAccessControlExtensionChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "adminNodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdminNodeID:@
setAdminNodeID :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setAdminNodeID mtrAccessControlClusterAccessControlExtensionChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "setAdminNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- adminPasscodeID@
adminPasscodeID :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id NSNumber)
adminPasscodeID mtrAccessControlClusterAccessControlExtensionChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "adminPasscodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdminPasscodeID:@
setAdminPasscodeID :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setAdminPasscodeID mtrAccessControlClusterAccessControlExtensionChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "setAdminPasscodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- changeType@
changeType :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id NSNumber)
changeType mtrAccessControlClusterAccessControlExtensionChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "changeType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChangeType:@
setChangeType :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setChangeType mtrAccessControlClusterAccessControlExtensionChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "setChangeType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latestValue@
latestValue :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id MTRAccessControlClusterAccessControlExtensionStruct)
latestValue mtrAccessControlClusterAccessControlExtensionChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "latestValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatestValue:@
setLatestValue :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsMTRAccessControlClusterAccessControlExtensionStruct value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setLatestValue mtrAccessControlClusterAccessControlExtensionChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "setLatestValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlExtensionChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlExtensionChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlExtensionChangedEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @adminNodeID@
adminNodeIDSelector :: Selector
adminNodeIDSelector = mkSelector "adminNodeID"

-- | @Selector@ for @setAdminNodeID:@
setAdminNodeIDSelector :: Selector
setAdminNodeIDSelector = mkSelector "setAdminNodeID:"

-- | @Selector@ for @adminPasscodeID@
adminPasscodeIDSelector :: Selector
adminPasscodeIDSelector = mkSelector "adminPasscodeID"

-- | @Selector@ for @setAdminPasscodeID:@
setAdminPasscodeIDSelector :: Selector
setAdminPasscodeIDSelector = mkSelector "setAdminPasscodeID:"

-- | @Selector@ for @changeType@
changeTypeSelector :: Selector
changeTypeSelector = mkSelector "changeType"

-- | @Selector@ for @setChangeType:@
setChangeTypeSelector :: Selector
setChangeTypeSelector = mkSelector "setChangeType:"

-- | @Selector@ for @latestValue@
latestValueSelector :: Selector
latestValueSelector = mkSelector "latestValue"

-- | @Selector@ for @setLatestValue:@
setLatestValueSelector :: Selector
setLatestValueSelector = mkSelector "setLatestValue:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

