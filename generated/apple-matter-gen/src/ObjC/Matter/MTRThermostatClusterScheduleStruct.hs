{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterScheduleStruct@.
module ObjC.Matter.MTRThermostatClusterScheduleStruct
  ( MTRThermostatClusterScheduleStruct
  , IsMTRThermostatClusterScheduleStruct(..)
  , scheduleHandle
  , setScheduleHandle
  , systemMode
  , setSystemMode
  , name
  , setName
  , presetHandle
  , setPresetHandle
  , transitions
  , setTransitions
  , builtIn
  , setBuiltIn
  , scheduleHandleSelector
  , setScheduleHandleSelector
  , systemModeSelector
  , setSystemModeSelector
  , nameSelector
  , setNameSelector
  , presetHandleSelector
  , setPresetHandleSelector
  , transitionsSelector
  , setTransitionsSelector
  , builtInSelector
  , setBuiltInSelector


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

-- | @- scheduleHandle@
scheduleHandle :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSData)
scheduleHandle mtrThermostatClusterScheduleStruct  =
    sendMsg mtrThermostatClusterScheduleStruct (mkSelector "scheduleHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScheduleHandle:@
setScheduleHandle :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSData value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setScheduleHandle mtrThermostatClusterScheduleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleStruct (mkSelector "setScheduleHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- systemMode@
systemMode :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSNumber)
systemMode mtrThermostatClusterScheduleStruct  =
    sendMsg mtrThermostatClusterScheduleStruct (mkSelector "systemMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSystemMode:@
setSystemMode :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSNumber value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setSystemMode mtrThermostatClusterScheduleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleStruct (mkSelector "setSystemMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSString)
name mtrThermostatClusterScheduleStruct  =
    sendMsg mtrThermostatClusterScheduleStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSString value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setName mtrThermostatClusterScheduleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSData)
presetHandle mtrThermostatClusterScheduleStruct  =
    sendMsg mtrThermostatClusterScheduleStruct (mkSelector "presetHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSData value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setPresetHandle mtrThermostatClusterScheduleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleStruct (mkSelector "setPresetHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitions@
transitions :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSArray)
transitions mtrThermostatClusterScheduleStruct  =
    sendMsg mtrThermostatClusterScheduleStruct (mkSelector "transitions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitions:@
setTransitions :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSArray value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setTransitions mtrThermostatClusterScheduleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleStruct (mkSelector "setTransitions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- builtIn@
builtIn :: IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct => mtrThermostatClusterScheduleStruct -> IO (Id NSNumber)
builtIn mtrThermostatClusterScheduleStruct  =
    sendMsg mtrThermostatClusterScheduleStruct (mkSelector "builtIn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBuiltIn:@
setBuiltIn :: (IsMTRThermostatClusterScheduleStruct mtrThermostatClusterScheduleStruct, IsNSNumber value) => mtrThermostatClusterScheduleStruct -> value -> IO ()
setBuiltIn mtrThermostatClusterScheduleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleStruct (mkSelector "setBuiltIn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scheduleHandle@
scheduleHandleSelector :: Selector
scheduleHandleSelector = mkSelector "scheduleHandle"

-- | @Selector@ for @setScheduleHandle:@
setScheduleHandleSelector :: Selector
setScheduleHandleSelector = mkSelector "setScheduleHandle:"

-- | @Selector@ for @systemMode@
systemModeSelector :: Selector
systemModeSelector = mkSelector "systemMode"

-- | @Selector@ for @setSystemMode:@
setSystemModeSelector :: Selector
setSystemModeSelector = mkSelector "setSystemMode:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @transitions@
transitionsSelector :: Selector
transitionsSelector = mkSelector "transitions"

-- | @Selector@ for @setTransitions:@
setTransitionsSelector :: Selector
setTransitionsSelector = mkSelector "setTransitions:"

-- | @Selector@ for @builtIn@
builtInSelector :: Selector
builtInSelector = mkSelector "builtIn"

-- | @Selector@ for @setBuiltIn:@
setBuiltInSelector :: Selector
setBuiltInSelector = mkSelector "setBuiltIn:"

