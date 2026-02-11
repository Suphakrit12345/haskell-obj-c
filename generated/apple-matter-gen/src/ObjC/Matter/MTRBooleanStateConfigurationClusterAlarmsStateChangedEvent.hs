{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBooleanStateConfigurationClusterAlarmsStateChangedEvent@.
module ObjC.Matter.MTRBooleanStateConfigurationClusterAlarmsStateChangedEvent
  ( MTRBooleanStateConfigurationClusterAlarmsStateChangedEvent
  , IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent(..)
  , alarmsActive
  , setAlarmsActive
  , alarmsSuppressed
  , setAlarmsSuppressed
  , alarmsActiveSelector
  , setAlarmsActiveSelector
  , alarmsSuppressedSelector
  , setAlarmsSuppressedSelector


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

-- | @- alarmsActive@
alarmsActive :: IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent => mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent -> IO (Id NSNumber)
alarmsActive mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent  =
    sendMsg mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent (mkSelector "alarmsActive") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlarmsActive:@
setAlarmsActive :: (IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent, IsNSNumber value) => mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent -> value -> IO ()
setAlarmsActive mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent (mkSelector "setAlarmsActive:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alarmsSuppressed@
alarmsSuppressed :: IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent => mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent -> IO (Id NSNumber)
alarmsSuppressed mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent  =
    sendMsg mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent (mkSelector "alarmsSuppressed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlarmsSuppressed:@
setAlarmsSuppressed :: (IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent, IsNSNumber value) => mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent -> value -> IO ()
setAlarmsSuppressed mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent (mkSelector "setAlarmsSuppressed:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmsActive@
alarmsActiveSelector :: Selector
alarmsActiveSelector = mkSelector "alarmsActive"

-- | @Selector@ for @setAlarmsActive:@
setAlarmsActiveSelector :: Selector
setAlarmsActiveSelector = mkSelector "setAlarmsActive:"

-- | @Selector@ for @alarmsSuppressed@
alarmsSuppressedSelector :: Selector
alarmsSuppressedSelector = mkSelector "alarmsSuppressed"

-- | @Selector@ for @setAlarmsSuppressed:@
setAlarmsSuppressedSelector :: Selector
setAlarmsSuppressedSelector = mkSelector "setAlarmsSuppressed:"

