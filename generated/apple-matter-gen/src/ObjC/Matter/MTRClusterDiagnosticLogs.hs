{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Diagnostic Logs    The cluster provides commands for retrieving unstructured diagnostic logs from a Node that may be used to aid in diagnostics.
--
-- Generated bindings for @MTRClusterDiagnosticLogs@.
module ObjC.Matter.MTRClusterDiagnosticLogs
  ( MTRClusterDiagnosticLogs
  , IsMTRClusterDiagnosticLogs(..)
  , retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completion:@
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDiagnosticLogs -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDiagnosticLogs  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDiagnosticLogs (mkSelector "retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDiagnosticLogs  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDiagnosticLogs (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDiagnosticLogs  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDiagnosticLogs (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDiagnosticLogs  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDiagnosticLogs (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDiagnosticLogs  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDiagnosticLogs (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDiagnosticLogs  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDiagnosticLogs (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs => mtrClusterDiagnosticLogs -> IO (Id MTRClusterDiagnosticLogs)
init_ mtrClusterDiagnosticLogs  =
    sendMsg mtrClusterDiagnosticLogs (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterDiagnosticLogs)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDiagnosticLogs"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRDevice device, IsNSObject queue) => mtrClusterDiagnosticLogs -> device -> CUShort -> queue -> IO (Id MTRClusterDiagnosticLogs)
initWithDevice_endpoint_queue mtrClusterDiagnosticLogs  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterDiagnosticLogs (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDiagnosticLogs -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDiagnosticLogs  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDiagnosticLogs (mkSelector "retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDiagnosticLogs -> device -> endpointID -> queue -> IO (Id MTRClusterDiagnosticLogs)
initWithDevice_endpointID_queue mtrClusterDiagnosticLogs  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterDiagnosticLogs (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completion:@
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completion:"

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

-- | @Selector@ for @retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

