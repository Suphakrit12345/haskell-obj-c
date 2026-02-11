{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Power Topology    The Power Topology Cluster provides a mechanism for expressing how power is flowing between endpoints.
--
-- Generated bindings for @MTRClusterPowerTopology@.
module ObjC.Matter.MTRClusterPowerTopology
  ( MTRClusterPowerTopology
  , IsMTRClusterPowerTopology(..)
  , readAttributeAvailableEndpointsWithParams
  , readAttributeActiveEndpointsWithParams
  , readAttributeElectricalCircuitNodesWithParams
  , writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval
  , writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeAvailableEndpointsWithParamsSelector
  , readAttributeActiveEndpointsWithParamsSelector
  , readAttributeElectricalCircuitNodesWithParamsSelector
  , writeAttributeElectricalCircuitNodesWithValue_expectedValueIntervalSelector
  , writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributeAvailableEndpointsWithParams:@
readAttributeAvailableEndpointsWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeAvailableEndpointsWithParams mtrClusterPowerTopology  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerTopology (mkSelector "readAttributeAvailableEndpointsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveEndpointsWithParams:@
readAttributeActiveEndpointsWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeActiveEndpointsWithParams mtrClusterPowerTopology  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerTopology (mkSelector "readAttributeActiveEndpointsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeElectricalCircuitNodesWithParams:@
readAttributeElectricalCircuitNodesWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeElectricalCircuitNodesWithParams mtrClusterPowerTopology  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerTopology (mkSelector "readAttributeElectricalCircuitNodesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:@
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPowerTopology -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval mtrClusterPowerTopology  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterPowerTopology (mkSelector "writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:params:@
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_params :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPowerTopology -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_params mtrClusterPowerTopology  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterPowerTopology (mkSelector "writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPowerTopology  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerTopology (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPowerTopology  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerTopology (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPowerTopology  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerTopology (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPowerTopology  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerTopology (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPowerTopology  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterPowerTopology (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterPowerTopology mtrClusterPowerTopology => mtrClusterPowerTopology -> IO (Id MTRClusterPowerTopology)
init_ mtrClusterPowerTopology  =
    sendMsg mtrClusterPowerTopology (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterPowerTopology)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPowerTopology"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPowerTopology -> device -> endpointID -> queue -> IO (Id MTRClusterPowerTopology)
initWithDevice_endpointID_queue mtrClusterPowerTopology  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterPowerTopology (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAvailableEndpointsWithParams:@
readAttributeAvailableEndpointsWithParamsSelector :: Selector
readAttributeAvailableEndpointsWithParamsSelector = mkSelector "readAttributeAvailableEndpointsWithParams:"

-- | @Selector@ for @readAttributeActiveEndpointsWithParams:@
readAttributeActiveEndpointsWithParamsSelector :: Selector
readAttributeActiveEndpointsWithParamsSelector = mkSelector "readAttributeActiveEndpointsWithParams:"

-- | @Selector@ for @readAttributeElectricalCircuitNodesWithParams:@
readAttributeElectricalCircuitNodesWithParamsSelector :: Selector
readAttributeElectricalCircuitNodesWithParamsSelector = mkSelector "readAttributeElectricalCircuitNodesWithParams:"

-- | @Selector@ for @writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:@
writeAttributeElectricalCircuitNodesWithValue_expectedValueIntervalSelector :: Selector
writeAttributeElectricalCircuitNodesWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:params:@
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:params:"

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

