{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlEntryChangedEvent@.
module ObjC.Matter.MTRAccessControlClusterAccessControlEntryChangedEvent
  ( MTRAccessControlClusterAccessControlEntryChangedEvent
  , IsMTRAccessControlClusterAccessControlEntryChangedEvent(..)
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
adminNodeID :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id NSNumber)
adminNodeID mtrAccessControlClusterAccessControlEntryChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "adminNodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdminNodeID:@
setAdminNodeID :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setAdminNodeID mtrAccessControlClusterAccessControlEntryChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "setAdminNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- adminPasscodeID@
adminPasscodeID :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id NSNumber)
adminPasscodeID mtrAccessControlClusterAccessControlEntryChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "adminPasscodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdminPasscodeID:@
setAdminPasscodeID :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setAdminPasscodeID mtrAccessControlClusterAccessControlEntryChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "setAdminPasscodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- changeType@
changeType :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id NSNumber)
changeType mtrAccessControlClusterAccessControlEntryChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "changeType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChangeType:@
setChangeType :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setChangeType mtrAccessControlClusterAccessControlEntryChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "setChangeType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latestValue@
latestValue :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id MTRAccessControlClusterAccessControlEntryStruct)
latestValue mtrAccessControlClusterAccessControlEntryChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "latestValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatestValue:@
setLatestValue :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsMTRAccessControlClusterAccessControlEntryStruct value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setLatestValue mtrAccessControlClusterAccessControlEntryChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "setLatestValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlEntryChangedEvent  =
    sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlEntryChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlEntryChangedEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

