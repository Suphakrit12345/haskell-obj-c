{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Software Diagnostics    The Software Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRClusterSoftwareDiagnostics@.
module ObjC.Matter.MTRClusterSoftwareDiagnostics
  ( MTRClusterSoftwareDiagnostics
  , IsMTRClusterSoftwareDiagnostics(..)
  , resetWatermarksWithParams_expectedValues_expectedValueInterval_completion
  , resetWatermarksWithExpectedValues_expectedValueInterval_completion
  , readAttributeThreadMetricsWithParams
  , readAttributeCurrentHeapFreeWithParams
  , readAttributeCurrentHeapUsedWithParams
  , readAttributeCurrentHeapHighWatermarkWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , resetWatermarksWithParams_expectedValues_expectedValueInterval_completionHandler
  , resetWatermarksWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , resetWatermarksWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetWatermarksWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeThreadMetricsWithParamsSelector
  , readAttributeCurrentHeapFreeWithParamsSelector
  , readAttributeCurrentHeapUsedWithParamsSelector
  , readAttributeCurrentHeapHighWatermarkWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , resetWatermarksWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , resetWatermarksWithExpectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- resetWatermarksWithParams:expectedValues:expectedValueInterval:completion:@
resetWatermarksWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRSoftwareDiagnosticsClusterResetWatermarksParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterSoftwareDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetWatermarksWithParams_expectedValues_expectedValueInterval_completion mtrClusterSoftwareDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterSoftwareDiagnostics (mkSelector "resetWatermarksWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetWatermarksWithExpectedValues:expectedValueInterval:completion:@
resetWatermarksWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterSoftwareDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetWatermarksWithExpectedValues_expectedValueInterval_completion mtrClusterSoftwareDiagnostics  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterSoftwareDiagnostics (mkSelector "resetWatermarksWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeThreadMetricsWithParams:@
readAttributeThreadMetricsWithParams :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRReadParams params) => mtrClusterSoftwareDiagnostics -> params -> IO (Id NSDictionary)
readAttributeThreadMetricsWithParams mtrClusterSoftwareDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoftwareDiagnostics (mkSelector "readAttributeThreadMetricsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentHeapFreeWithParams:@
readAttributeCurrentHeapFreeWithParams :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRReadParams params) => mtrClusterSoftwareDiagnostics -> params -> IO (Id NSDictionary)
readAttributeCurrentHeapFreeWithParams mtrClusterSoftwareDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoftwareDiagnostics (mkSelector "readAttributeCurrentHeapFreeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentHeapUsedWithParams:@
readAttributeCurrentHeapUsedWithParams :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRReadParams params) => mtrClusterSoftwareDiagnostics -> params -> IO (Id NSDictionary)
readAttributeCurrentHeapUsedWithParams mtrClusterSoftwareDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoftwareDiagnostics (mkSelector "readAttributeCurrentHeapUsedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentHeapHighWatermarkWithParams:@
readAttributeCurrentHeapHighWatermarkWithParams :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRReadParams params) => mtrClusterSoftwareDiagnostics -> params -> IO (Id NSDictionary)
readAttributeCurrentHeapHighWatermarkWithParams mtrClusterSoftwareDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoftwareDiagnostics (mkSelector "readAttributeCurrentHeapHighWatermarkWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRReadParams params) => mtrClusterSoftwareDiagnostics -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterSoftwareDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoftwareDiagnostics (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRReadParams params) => mtrClusterSoftwareDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterSoftwareDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoftwareDiagnostics (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRReadParams params) => mtrClusterSoftwareDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterSoftwareDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoftwareDiagnostics (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRReadParams params) => mtrClusterSoftwareDiagnostics -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterSoftwareDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoftwareDiagnostics (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRReadParams params) => mtrClusterSoftwareDiagnostics -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterSoftwareDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSoftwareDiagnostics (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics => mtrClusterSoftwareDiagnostics -> IO (Id MTRClusterSoftwareDiagnostics)
init_ mtrClusterSoftwareDiagnostics  =
    sendMsg mtrClusterSoftwareDiagnostics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterSoftwareDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRClusterSoftwareDiagnostics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRDevice device, IsNSObject queue) => mtrClusterSoftwareDiagnostics -> device -> CUShort -> queue -> IO (Id MTRClusterSoftwareDiagnostics)
initWithDevice_endpoint_queue mtrClusterSoftwareDiagnostics  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterSoftwareDiagnostics (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- resetWatermarksWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetWatermarksWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRSoftwareDiagnosticsClusterResetWatermarksParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterSoftwareDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetWatermarksWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterSoftwareDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterSoftwareDiagnostics (mkSelector "resetWatermarksWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resetWatermarksWithExpectedValues:expectedValueInterval:completionHandler:@
resetWatermarksWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterSoftwareDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetWatermarksWithExpectedValues_expectedValueInterval_completionHandler mtrClusterSoftwareDiagnostics  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterSoftwareDiagnostics (mkSelector "resetWatermarksWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterSoftwareDiagnostics mtrClusterSoftwareDiagnostics, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterSoftwareDiagnostics -> device -> endpointID -> queue -> IO (Id MTRClusterSoftwareDiagnostics)
initWithDevice_endpointID_queue mtrClusterSoftwareDiagnostics  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterSoftwareDiagnostics (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetWatermarksWithParams:expectedValues:expectedValueInterval:completion:@
resetWatermarksWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resetWatermarksWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetWatermarksWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetWatermarksWithExpectedValues:expectedValueInterval:completion:@
resetWatermarksWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resetWatermarksWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetWatermarksWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeThreadMetricsWithParams:@
readAttributeThreadMetricsWithParamsSelector :: Selector
readAttributeThreadMetricsWithParamsSelector = mkSelector "readAttributeThreadMetricsWithParams:"

-- | @Selector@ for @readAttributeCurrentHeapFreeWithParams:@
readAttributeCurrentHeapFreeWithParamsSelector :: Selector
readAttributeCurrentHeapFreeWithParamsSelector = mkSelector "readAttributeCurrentHeapFreeWithParams:"

-- | @Selector@ for @readAttributeCurrentHeapUsedWithParams:@
readAttributeCurrentHeapUsedWithParamsSelector :: Selector
readAttributeCurrentHeapUsedWithParamsSelector = mkSelector "readAttributeCurrentHeapUsedWithParams:"

-- | @Selector@ for @readAttributeCurrentHeapHighWatermarkWithParams:@
readAttributeCurrentHeapHighWatermarkWithParamsSelector :: Selector
readAttributeCurrentHeapHighWatermarkWithParamsSelector = mkSelector "readAttributeCurrentHeapHighWatermarkWithParams:"

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

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @resetWatermarksWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetWatermarksWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
resetWatermarksWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetWatermarksWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @resetWatermarksWithExpectedValues:expectedValueInterval:completionHandler:@
resetWatermarksWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
resetWatermarksWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetWatermarksWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

