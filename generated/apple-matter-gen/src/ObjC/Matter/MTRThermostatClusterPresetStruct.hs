{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterPresetStruct@.
module ObjC.Matter.MTRThermostatClusterPresetStruct
  ( MTRThermostatClusterPresetStruct
  , IsMTRThermostatClusterPresetStruct(..)
  , presetHandle
  , setPresetHandle
  , presetScenario
  , setPresetScenario
  , name
  , setName
  , coolingSetpoint
  , setCoolingSetpoint
  , heatingSetpoint
  , setHeatingSetpoint
  , builtIn
  , setBuiltIn
  , presetHandleSelector
  , setPresetHandleSelector
  , presetScenarioSelector
  , setPresetScenarioSelector
  , nameSelector
  , setNameSelector
  , coolingSetpointSelector
  , setCoolingSetpointSelector
  , heatingSetpointSelector
  , setHeatingSetpointSelector
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

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSData)
presetHandle mtrThermostatClusterPresetStruct  =
    sendMsg mtrThermostatClusterPresetStruct (mkSelector "presetHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSData value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setPresetHandle mtrThermostatClusterPresetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterPresetStruct (mkSelector "setPresetHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- presetScenario@
presetScenario :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSNumber)
presetScenario mtrThermostatClusterPresetStruct  =
    sendMsg mtrThermostatClusterPresetStruct (mkSelector "presetScenario") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetScenario:@
setPresetScenario :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSNumber value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setPresetScenario mtrThermostatClusterPresetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterPresetStruct (mkSelector "setPresetScenario:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSString)
name mtrThermostatClusterPresetStruct  =
    sendMsg mtrThermostatClusterPresetStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSString value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setName mtrThermostatClusterPresetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterPresetStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- coolingSetpoint@
coolingSetpoint :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSNumber)
coolingSetpoint mtrThermostatClusterPresetStruct  =
    sendMsg mtrThermostatClusterPresetStruct (mkSelector "coolingSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCoolingSetpoint:@
setCoolingSetpoint :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSNumber value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setCoolingSetpoint mtrThermostatClusterPresetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterPresetStruct (mkSelector "setCoolingSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- heatingSetpoint@
heatingSetpoint :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSNumber)
heatingSetpoint mtrThermostatClusterPresetStruct  =
    sendMsg mtrThermostatClusterPresetStruct (mkSelector "heatingSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeatingSetpoint:@
setHeatingSetpoint :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSNumber value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setHeatingSetpoint mtrThermostatClusterPresetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterPresetStruct (mkSelector "setHeatingSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- builtIn@
builtIn :: IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct => mtrThermostatClusterPresetStruct -> IO (Id NSNumber)
builtIn mtrThermostatClusterPresetStruct  =
    sendMsg mtrThermostatClusterPresetStruct (mkSelector "builtIn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBuiltIn:@
setBuiltIn :: (IsMTRThermostatClusterPresetStruct mtrThermostatClusterPresetStruct, IsNSNumber value) => mtrThermostatClusterPresetStruct -> value -> IO ()
setBuiltIn mtrThermostatClusterPresetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterPresetStruct (mkSelector "setBuiltIn:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @presetScenario@
presetScenarioSelector :: Selector
presetScenarioSelector = mkSelector "presetScenario"

-- | @Selector@ for @setPresetScenario:@
setPresetScenarioSelector :: Selector
setPresetScenarioSelector = mkSelector "setPresetScenario:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @coolingSetpoint@
coolingSetpointSelector :: Selector
coolingSetpointSelector = mkSelector "coolingSetpoint"

-- | @Selector@ for @setCoolingSetpoint:@
setCoolingSetpointSelector :: Selector
setCoolingSetpointSelector = mkSelector "setCoolingSetpoint:"

-- | @Selector@ for @heatingSetpoint@
heatingSetpointSelector :: Selector
heatingSetpointSelector = mkSelector "heatingSetpoint"

-- | @Selector@ for @setHeatingSetpoint:@
setHeatingSetpointSelector :: Selector
setHeatingSetpointSelector = mkSelector "setHeatingSetpoint:"

-- | @Selector@ for @builtIn@
builtInSelector :: Selector
builtInSelector = mkSelector "builtIn"

-- | @Selector@ for @setBuiltIn:@
setBuiltInSelector :: Selector
setBuiltInSelector = mkSelector "setBuiltIn:"

