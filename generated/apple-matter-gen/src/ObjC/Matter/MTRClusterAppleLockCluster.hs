{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Apple Lock Cluster    This lock cluster provides Apple-specific information/configuration for the lock.
--
-- Generated bindings for @MTRClusterAppleLockCluster@.
module ObjC.Matter.MTRClusterAppleLockCluster
  ( MTRClusterAppleLockCluster
  , IsMTRClusterAppleLockCluster(..)
  , readAttributeIntentDetectionWithParams
  , writeAttributeIntentDetectionWithValue_expectedValueInterval
  , writeAttributeIntentDetectionWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , readAttributeIntentDetectionWithParamsSelector
  , writeAttributeIntentDetectionWithValue_expectedValueIntervalSelector
  , writeAttributeIntentDetectionWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributeIntentDetectionWithParams:@
readAttributeIntentDetectionWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeIntentDetectionWithParams mtrClusterAppleLockCluster  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleLockCluster (mkSelector "readAttributeIntentDetectionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeIntentDetectionWithValue:expectedValueInterval:@
writeAttributeIntentDetectionWithValue_expectedValueInterval :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterAppleLockCluster -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeIntentDetectionWithValue_expectedValueInterval mtrClusterAppleLockCluster  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterAppleLockCluster (mkSelector "writeAttributeIntentDetectionWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeIntentDetectionWithValue:expectedValueInterval:params:@
writeAttributeIntentDetectionWithValue_expectedValueInterval_params :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterAppleLockCluster -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeIntentDetectionWithValue_expectedValueInterval_params mtrClusterAppleLockCluster  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterAppleLockCluster (mkSelector "writeAttributeIntentDetectionWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAppleLockCluster  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleLockCluster (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAppleLockCluster  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleLockCluster (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAppleLockCluster  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleLockCluster (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAppleLockCluster  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleLockCluster (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAppleLockCluster  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAppleLockCluster (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster => mtrClusterAppleLockCluster -> IO (Id MTRClusterAppleLockCluster)
init_ mtrClusterAppleLockCluster  =
    sendMsg mtrClusterAppleLockCluster (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterAppleLockCluster)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAppleLockCluster"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAppleLockCluster -> device -> endpointID -> queue -> IO (Id MTRClusterAppleLockCluster)
initWithDevice_endpointID_queue mtrClusterAppleLockCluster  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterAppleLockCluster (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeIntentDetectionWithParams:@
readAttributeIntentDetectionWithParamsSelector :: Selector
readAttributeIntentDetectionWithParamsSelector = mkSelector "readAttributeIntentDetectionWithParams:"

-- | @Selector@ for @writeAttributeIntentDetectionWithValue:expectedValueInterval:@
writeAttributeIntentDetectionWithValue_expectedValueIntervalSelector :: Selector
writeAttributeIntentDetectionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeIntentDetectionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeIntentDetectionWithValue:expectedValueInterval:params:@
writeAttributeIntentDetectionWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeIntentDetectionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeIntentDetectionWithValue:expectedValueInterval:params:"

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

