{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterTimeZoneStatusEvent@.
module ObjC.Matter.MTRTimeSynchronizationClusterTimeZoneStatusEvent
  ( MTRTimeSynchronizationClusterTimeZoneStatusEvent
  , IsMTRTimeSynchronizationClusterTimeZoneStatusEvent(..)
  , offset
  , setOffset
  , name
  , setName
  , offsetSelector
  , setOffsetSelector
  , nameSelector
  , setNameSelector


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

-- | @- offset@
offset :: IsMTRTimeSynchronizationClusterTimeZoneStatusEvent mtrTimeSynchronizationClusterTimeZoneStatusEvent => mtrTimeSynchronizationClusterTimeZoneStatusEvent -> IO (Id NSNumber)
offset mtrTimeSynchronizationClusterTimeZoneStatusEvent  =
    sendMsg mtrTimeSynchronizationClusterTimeZoneStatusEvent (mkSelector "offset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffset:@
setOffset :: (IsMTRTimeSynchronizationClusterTimeZoneStatusEvent mtrTimeSynchronizationClusterTimeZoneStatusEvent, IsNSNumber value) => mtrTimeSynchronizationClusterTimeZoneStatusEvent -> value -> IO ()
setOffset mtrTimeSynchronizationClusterTimeZoneStatusEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTimeZoneStatusEvent (mkSelector "setOffset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRTimeSynchronizationClusterTimeZoneStatusEvent mtrTimeSynchronizationClusterTimeZoneStatusEvent => mtrTimeSynchronizationClusterTimeZoneStatusEvent -> IO (Id NSString)
name mtrTimeSynchronizationClusterTimeZoneStatusEvent  =
    sendMsg mtrTimeSynchronizationClusterTimeZoneStatusEvent (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRTimeSynchronizationClusterTimeZoneStatusEvent mtrTimeSynchronizationClusterTimeZoneStatusEvent, IsNSString value) => mtrTimeSynchronizationClusterTimeZoneStatusEvent -> value -> IO ()
setName mtrTimeSynchronizationClusterTimeZoneStatusEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTimeZoneStatusEvent (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

