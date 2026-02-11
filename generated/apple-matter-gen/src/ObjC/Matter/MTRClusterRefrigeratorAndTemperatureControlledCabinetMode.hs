{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Refrigerator And Temperature Controlled Cabinet Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterRefrigeratorAndTemperatureControlledCabinetMode@.
module ObjC.Matter.MTRClusterRefrigeratorAndTemperatureControlledCabinetMode
  ( MTRClusterRefrigeratorAndTemperatureControlledCabinetMode
  , IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode(..)
  , changeToModeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSupportedModesWithParams
  , readAttributeCurrentModeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeSupportedModesWithParamsSelector
  , readAttributeCurrentModeWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpointID_queueSelector


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

-- | @- changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeSupportedModesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeCurrentModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> IO (Id MTRClusterRefrigeratorAndTemperatureControlledCabinetMode)
init_ mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  =
    sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterRefrigeratorAndTemperatureControlledCabinetMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> device -> endpointID -> queue -> IO (Id MTRClusterRefrigeratorAndTemperatureControlledCabinetMode)
initWithDevice_endpointID_queue mtrClusterRefrigeratorAndTemperatureControlledCabinetMode  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterRefrigeratorAndTemperatureControlledCabinetMode (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParamsSelector :: Selector
readAttributeSupportedModesWithParamsSelector = mkSelector "readAttributeSupportedModesWithParams:"

-- | @Selector@ for @readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParamsSelector :: Selector
readAttributeCurrentModeWithParamsSelector = mkSelector "readAttributeCurrentModeWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

