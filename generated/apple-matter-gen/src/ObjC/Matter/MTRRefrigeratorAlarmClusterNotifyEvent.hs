{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRefrigeratorAlarmClusterNotifyEvent@.
module ObjC.Matter.MTRRefrigeratorAlarmClusterNotifyEvent
  ( MTRRefrigeratorAlarmClusterNotifyEvent
  , IsMTRRefrigeratorAlarmClusterNotifyEvent(..)
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
active :: IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent => mtrRefrigeratorAlarmClusterNotifyEvent -> IO (Id NSNumber)
active mtrRefrigeratorAlarmClusterNotifyEvent  =
    sendMsg mtrRefrigeratorAlarmClusterNotifyEvent (mkSelector "active") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActive:@
setActive :: (IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent, IsNSNumber value) => mtrRefrigeratorAlarmClusterNotifyEvent -> value -> IO ()
setActive mtrRefrigeratorAlarmClusterNotifyEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrRefrigeratorAlarmClusterNotifyEvent (mkSelector "setActive:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- inactive@
inactive :: IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent => mtrRefrigeratorAlarmClusterNotifyEvent -> IO (Id NSNumber)
inactive mtrRefrigeratorAlarmClusterNotifyEvent  =
    sendMsg mtrRefrigeratorAlarmClusterNotifyEvent (mkSelector "inactive") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInactive:@
setInactive :: (IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent, IsNSNumber value) => mtrRefrigeratorAlarmClusterNotifyEvent -> value -> IO ()
setInactive mtrRefrigeratorAlarmClusterNotifyEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrRefrigeratorAlarmClusterNotifyEvent (mkSelector "setInactive:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent => mtrRefrigeratorAlarmClusterNotifyEvent -> IO (Id NSNumber)
state mtrRefrigeratorAlarmClusterNotifyEvent  =
    sendMsg mtrRefrigeratorAlarmClusterNotifyEvent (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setState:@
setState :: (IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent, IsNSNumber value) => mtrRefrigeratorAlarmClusterNotifyEvent -> value -> IO ()
setState mtrRefrigeratorAlarmClusterNotifyEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrRefrigeratorAlarmClusterNotifyEvent (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mask@
mask :: IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent => mtrRefrigeratorAlarmClusterNotifyEvent -> IO (Id NSNumber)
mask mtrRefrigeratorAlarmClusterNotifyEvent  =
    sendMsg mtrRefrigeratorAlarmClusterNotifyEvent (mkSelector "mask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMask:@
setMask :: (IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent, IsNSNumber value) => mtrRefrigeratorAlarmClusterNotifyEvent -> value -> IO ()
setMask mtrRefrigeratorAlarmClusterNotifyEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrRefrigeratorAlarmClusterNotifyEvent (mkSelector "setMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

