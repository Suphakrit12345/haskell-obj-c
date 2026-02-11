{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Sample MEI    The Sample MEI cluster showcases a cluster manufacturer extensions
--
-- Generated bindings for @MTRClusterSampleMEI@.
module ObjC.Matter.MTRClusterSampleMEI
  ( MTRClusterSampleMEI
  , IsMTRClusterSampleMEI(..)
  , pingWithParams_expectedValues_expectedValueInterval_completion
  , pingWithExpectedValues_expectedValueInterval_completion
  , addArgumentsWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeFlipFlopWithParams
  , writeAttributeFlipFlopWithValue_expectedValueInterval
  , writeAttributeFlipFlopWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , pingWithParams_expectedValues_expectedValueInterval_completionSelector
  , pingWithExpectedValues_expectedValueInterval_completionSelector
  , addArgumentsWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeFlipFlopWithParamsSelector
  , writeAttributeFlipFlopWithValue_expectedValueIntervalSelector
  , writeAttributeFlipFlopWithValue_expectedValueInterval_paramsSelector
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

-- | @- pingWithParams:expectedValues:expectedValueInterval:completion:@
pingWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRSampleMEIClusterPingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterSampleMEI -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pingWithParams_expectedValues_expectedValueInterval_completion mtrClusterSampleMEI  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterSampleMEI (mkSelector "pingWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pingWithExpectedValues:expectedValueInterval:completion:@
pingWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterSampleMEI -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pingWithExpectedValues_expectedValueInterval_completion mtrClusterSampleMEI  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterSampleMEI (mkSelector "pingWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addArgumentsWithParams:expectedValues:expectedValueInterval:completion:@
addArgumentsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRSampleMEIClusterAddArgumentsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterSampleMEI -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addArgumentsWithParams_expectedValues_expectedValueInterval_completion mtrClusterSampleMEI  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterSampleMEI (mkSelector "addArgumentsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFlipFlopWithParams:@
readAttributeFlipFlopWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeFlipFlopWithParams mtrClusterSampleMEI  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSampleMEI (mkSelector "readAttributeFlipFlopWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeFlipFlopWithValue:expectedValueInterval:@
writeAttributeFlipFlopWithValue_expectedValueInterval :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterSampleMEI -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeFlipFlopWithValue_expectedValueInterval mtrClusterSampleMEI  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterSampleMEI (mkSelector "writeAttributeFlipFlopWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeFlipFlopWithValue:expectedValueInterval:params:@
writeAttributeFlipFlopWithValue_expectedValueInterval_params :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterSampleMEI -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeFlipFlopWithValue_expectedValueInterval_params mtrClusterSampleMEI  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterSampleMEI (mkSelector "writeAttributeFlipFlopWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterSampleMEI  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSampleMEI (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterSampleMEI  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSampleMEI (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterSampleMEI  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSampleMEI (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterSampleMEI  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSampleMEI (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterSampleMEI  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSampleMEI (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterSampleMEI mtrClusterSampleMEI => mtrClusterSampleMEI -> IO (Id MTRClusterSampleMEI)
init_ mtrClusterSampleMEI  =
    sendMsg mtrClusterSampleMEI (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterSampleMEI)
new  =
  do
    cls' <- getRequiredClass "MTRClusterSampleMEI"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterSampleMEI -> device -> endpointID -> queue -> IO (Id MTRClusterSampleMEI)
initWithDevice_endpointID_queue mtrClusterSampleMEI  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterSampleMEI (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pingWithParams:expectedValues:expectedValueInterval:completion:@
pingWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
pingWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pingWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pingWithExpectedValues:expectedValueInterval:completion:@
pingWithExpectedValues_expectedValueInterval_completionSelector :: Selector
pingWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "pingWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addArgumentsWithParams:expectedValues:expectedValueInterval:completion:@
addArgumentsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addArgumentsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addArgumentsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeFlipFlopWithParams:@
readAttributeFlipFlopWithParamsSelector :: Selector
readAttributeFlipFlopWithParamsSelector = mkSelector "readAttributeFlipFlopWithParams:"

-- | @Selector@ for @writeAttributeFlipFlopWithValue:expectedValueInterval:@
writeAttributeFlipFlopWithValue_expectedValueIntervalSelector :: Selector
writeAttributeFlipFlopWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeFlipFlopWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeFlipFlopWithValue:expectedValueInterval:params:@
writeAttributeFlipFlopWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeFlipFlopWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeFlipFlopWithValue:expectedValueInterval:params:"

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

