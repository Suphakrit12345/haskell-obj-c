{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDishwasherAlarmClusterNotifyEvent@.
module ObjC.Matter.MTRDishwasherAlarmClusterNotifyEvent
  ( MTRDishwasherAlarmClusterNotifyEvent
  , IsMTRDishwasherAlarmClusterNotifyEvent(..)
  , active
  , setActive
  , inactive
  , setInactive
  , state
  , setState
  , mask
  , setMask
  , activeSelector
  , setActiveSelector
  , inactiveSelector
  , setInactiveSelector
  , stateSelector
  , setStateSelector
  , maskSelector
  , setMaskSelector


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

-- | @- active@
active :: IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent => mtrDishwasherAlarmClusterNotifyEvent -> IO (Id NSNumber)
active mtrDishwasherAlarmClusterNotifyEvent  =
    sendMsg mtrDishwasherAlarmClusterNotifyEvent (mkSelector "active") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActive:@
setActive :: (IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent, IsNSNumber value) => mtrDishwasherAlarmClusterNotifyEvent -> value -> IO ()
setActive mtrDishwasherAlarmClusterNotifyEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherAlarmClusterNotifyEvent (mkSelector "setActive:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- inactive@
inactive :: IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent => mtrDishwasherAlarmClusterNotifyEvent -> IO (Id NSNumber)
inactive mtrDishwasherAlarmClusterNotifyEvent  =
    sendMsg mtrDishwasherAlarmClusterNotifyEvent (mkSelector "inactive") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInactive:@
setInactive :: (IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent, IsNSNumber value) => mtrDishwasherAlarmClusterNotifyEvent -> value -> IO ()
setInactive mtrDishwasherAlarmClusterNotifyEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherAlarmClusterNotifyEvent (mkSelector "setInactive:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent => mtrDishwasherAlarmClusterNotifyEvent -> IO (Id NSNumber)
state mtrDishwasherAlarmClusterNotifyEvent  =
    sendMsg mtrDishwasherAlarmClusterNotifyEvent (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setState:@
setState :: (IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent, IsNSNumber value) => mtrDishwasherAlarmClusterNotifyEvent -> value -> IO ()
setState mtrDishwasherAlarmClusterNotifyEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherAlarmClusterNotifyEvent (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mask@
mask :: IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent => mtrDishwasherAlarmClusterNotifyEvent -> IO (Id NSNumber)
mask mtrDishwasherAlarmClusterNotifyEvent  =
    sendMsg mtrDishwasherAlarmClusterNotifyEvent (mkSelector "mask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMask:@
setMask :: (IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent, IsNSNumber value) => mtrDishwasherAlarmClusterNotifyEvent -> value -> IO ()
setMask mtrDishwasherAlarmClusterNotifyEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherAlarmClusterNotifyEvent (mkSelector "setMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @inactive@
inactiveSelector :: Selector
inactiveSelector = mkSelector "inactive"

-- | @Selector@ for @setInactive:@
setInactiveSelector :: Selector
setInactiveSelector = mkSelector "setInactive:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @mask@
maskSelector :: Selector
maskSelector = mkSelector "mask"

-- | @Selector@ for @setMask:@
setMaskSelector :: Selector
setMaskSelector = mkSelector "setMask:"

