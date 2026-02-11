{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Mode Select    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterModeSelect@.
module ObjC.Matter.MTRClusterModeSelect
  ( MTRClusterModeSelect
  , IsMTRClusterModeSelect(..)
  , changeToModeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeDescriptionWithParams
  , readAttributeStandardNamespaceWithParams
  , readAttributeSupportedModesWithParams
  , readAttributeCurrentModeWithParams
  , readAttributeStartUpModeWithParams
  , writeAttributeStartUpModeWithValue_expectedValueInterval
  , writeAttributeStartUpModeWithValue_expectedValueInterval_params
  , readAttributeOnModeWithParams
  , writeAttributeOnModeWithValue_expectedValueInterval
  , writeAttributeOnModeWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , changeToModeWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeDescriptionWithParamsSelector
  , readAttributeStandardNamespaceWithParamsSelector
  , readAttributeSupportedModesWithParamsSelector
  , readAttributeCurrentModeWithParamsSelector
  , readAttributeStartUpModeWithParamsSelector
  , writeAttributeStartUpModeWithValue_expectedValueIntervalSelector
  , writeAttributeStartUpModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeOnModeWithParamsSelector
  , writeAttributeOnModeWithValue_expectedValueIntervalSelector
  , writeAttributeOnModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , changeToModeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRModeSelectClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterModeSelect -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterModeSelect  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterModeSelect (mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeDescriptionWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeDescriptionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStandardNamespaceWithParams:@
readAttributeStandardNamespaceWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeStandardNamespaceWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeStandardNamespaceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeSupportedModesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeCurrentModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStartUpModeWithParams:@
readAttributeStartUpModeWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeStartUpModeWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeStartUpModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeStartUpModeWithValue:expectedValueInterval:@
writeAttributeStartUpModeWithValue_expectedValueInterval :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterModeSelect -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeStartUpModeWithValue_expectedValueInterval mtrClusterModeSelect  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterModeSelect (mkSelector "writeAttributeStartUpModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeStartUpModeWithValue:expectedValueInterval:params:@
writeAttributeStartUpModeWithValue_expectedValueInterval_params :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterModeSelect -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeStartUpModeWithValue_expectedValueInterval_params mtrClusterModeSelect  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterModeSelect (mkSelector "writeAttributeStartUpModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeOnModeWithParams:@
readAttributeOnModeWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeOnModeWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeOnModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeOnModeWithValue:expectedValueInterval:@
writeAttributeOnModeWithValue_expectedValueInterval :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterModeSelect -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnModeWithValue_expectedValueInterval mtrClusterModeSelect  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterModeSelect (mkSelector "writeAttributeOnModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeOnModeWithValue:expectedValueInterval:params:@
writeAttributeOnModeWithValue_expectedValueInterval_params :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterModeSelect -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnModeWithValue_expectedValueInterval_params mtrClusterModeSelect  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterModeSelect (mkSelector "writeAttributeOnModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterModeSelect  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterModeSelect (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterModeSelect mtrClusterModeSelect => mtrClusterModeSelect -> IO (Id MTRClusterModeSelect)
init_ mtrClusterModeSelect  =
    sendMsg mtrClusterModeSelect (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterModeSelect)
new  =
  do
    cls' <- getRequiredClass "MTRClusterModeSelect"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRDevice device, IsNSObject queue) => mtrClusterModeSelect -> device -> CUShort -> queue -> IO (Id MTRClusterModeSelect)
initWithDevice_endpoint_queue mtrClusterModeSelect  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterModeSelect (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- changeToModeWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRModeSelectClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterModeSelect -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterModeSelect  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterModeSelect (mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterModeSelect -> device -> endpointID -> queue -> IO (Id MTRClusterModeSelect)
initWithDevice_endpointID_queue mtrClusterModeSelect  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterModeSelect (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParamsSelector :: Selector
readAttributeDescriptionWithParamsSelector = mkSelector "readAttributeDescriptionWithParams:"

-- | @Selector@ for @readAttributeStandardNamespaceWithParams:@
readAttributeStandardNamespaceWithParamsSelector :: Selector
readAttributeStandardNamespaceWithParamsSelector = mkSelector "readAttributeStandardNamespaceWithParams:"

-- | @Selector@ for @readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParamsSelector :: Selector
readAttributeSupportedModesWithParamsSelector = mkSelector "readAttributeSupportedModesWithParams:"

-- | @Selector@ for @readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParamsSelector :: Selector
readAttributeCurrentModeWithParamsSelector = mkSelector "readAttributeCurrentModeWithParams:"

-- | @Selector@ for @readAttributeStartUpModeWithParams:@
readAttributeStartUpModeWithParamsSelector :: Selector
readAttributeStartUpModeWithParamsSelector = mkSelector "readAttributeStartUpModeWithParams:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:expectedValueInterval:@
writeAttributeStartUpModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeStartUpModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeStartUpModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:expectedValueInterval:params:@
writeAttributeStartUpModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeStartUpModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeStartUpModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOnModeWithParams:@
readAttributeOnModeWithParamsSelector :: Selector
readAttributeOnModeWithParamsSelector = mkSelector "readAttributeOnModeWithParams:"

-- | @Selector@ for @writeAttributeOnModeWithValue:expectedValueInterval:@
writeAttributeOnModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeOnModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnModeWithValue:expectedValueInterval:params:@
writeAttributeOnModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeOnModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnModeWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
changeToModeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

