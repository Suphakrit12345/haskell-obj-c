{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Network Directory    Manages the names and credentials of Thread networks visible to the user.
--
-- Generated bindings for @MTRClusterThreadNetworkDirectory@.
module ObjC.Matter.MTRClusterThreadNetworkDirectory
  ( MTRClusterThreadNetworkDirectory
  , IsMTRClusterThreadNetworkDirectory(..)
  , addNetworkWithParams_expectedValues_expectedValueInterval_completion
  , removeNetworkWithParams_expectedValues_expectedValueInterval_completion
  , getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completion
  , readAttributePreferredExtendedPanIDWithParams
  , writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval
  , writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_params
  , readAttributeThreadNetworksWithParams
  , readAttributeThreadNetworkTableSizeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addNetworkWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector
  , getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributePreferredExtendedPanIDWithParamsSelector
  , writeAttributePreferredExtendedPanIDWithValue_expectedValueIntervalSelector
  , writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_paramsSelector
  , readAttributeThreadNetworksWithParamsSelector
  , readAttributeThreadNetworkTableSizeWithParamsSelector
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

-- | @- addNetworkWithParams:expectedValues:expectedValueInterval:completion:@
addNetworkWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterAddNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDirectory -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addNetworkWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDirectory  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThreadNetworkDirectory (mkSelector "addNetworkWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeNetworkWithParams:expectedValues:expectedValueInterval:completion:@
removeNetworkWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDirectory -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeNetworkWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDirectory  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThreadNetworkDirectory (mkSelector "removeNetworkWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getOperationalDatasetWithParams:expectedValues:expectedValueInterval:completion:@
getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterGetOperationalDatasetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDirectory -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDirectory  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThreadNetworkDirectory (mkSelector "getOperationalDatasetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePreferredExtendedPanIDWithParams:@
readAttributePreferredExtendedPanIDWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributePreferredExtendedPanIDWithParams mtrClusterThreadNetworkDirectory  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDirectory (mkSelector "readAttributePreferredExtendedPanIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:@
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDirectory -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval mtrClusterThreadNetworkDirectory  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThreadNetworkDirectory (mkSelector "writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:params:@
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_params :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThreadNetworkDirectory -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_params mtrClusterThreadNetworkDirectory  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThreadNetworkDirectory (mkSelector "writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeThreadNetworksWithParams:@
readAttributeThreadNetworksWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeThreadNetworksWithParams mtrClusterThreadNetworkDirectory  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDirectory (mkSelector "readAttributeThreadNetworksWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeThreadNetworkTableSizeWithParams:@
readAttributeThreadNetworkTableSizeWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeThreadNetworkTableSizeWithParams mtrClusterThreadNetworkDirectory  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDirectory (mkSelector "readAttributeThreadNetworkTableSizeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterThreadNetworkDirectory  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDirectory (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterThreadNetworkDirectory  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDirectory (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterThreadNetworkDirectory  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDirectory (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterThreadNetworkDirectory  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDirectory (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterThreadNetworkDirectory  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDirectory (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory => mtrClusterThreadNetworkDirectory -> IO (Id MTRClusterThreadNetworkDirectory)
init_ mtrClusterThreadNetworkDirectory  =
    sendMsg mtrClusterThreadNetworkDirectory (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterThreadNetworkDirectory)
new  =
  do
    cls' <- getRequiredClass "MTRClusterThreadNetworkDirectory"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterThreadNetworkDirectory -> device -> endpointID -> queue -> IO (Id MTRClusterThreadNetworkDirectory)
initWithDevice_endpointID_queue mtrClusterThreadNetworkDirectory  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterThreadNetworkDirectory (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addNetworkWithParams:expectedValues:expectedValueInterval:completion:@
addNetworkWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addNetworkWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addNetworkWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeNetworkWithParams:expectedValues:expectedValueInterval:completion:@
removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeNetworkWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getOperationalDatasetWithParams:expectedValues:expectedValueInterval:completion:@
getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getOperationalDatasetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributePreferredExtendedPanIDWithParams:@
readAttributePreferredExtendedPanIDWithParamsSelector :: Selector
readAttributePreferredExtendedPanIDWithParamsSelector = mkSelector "readAttributePreferredExtendedPanIDWithParams:"

-- | @Selector@ for @writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:@
writeAttributePreferredExtendedPanIDWithValue_expectedValueIntervalSelector :: Selector
writeAttributePreferredExtendedPanIDWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:params:@
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeThreadNetworksWithParams:@
readAttributeThreadNetworksWithParamsSelector :: Selector
readAttributeThreadNetworksWithParamsSelector = mkSelector "readAttributeThreadNetworksWithParams:"

-- | @Selector@ for @readAttributeThreadNetworkTableSizeWithParams:@
readAttributeThreadNetworkTableSizeWithParamsSelector :: Selector
readAttributeThreadNetworkTableSizeWithParamsSelector = mkSelector "readAttributeThreadNetworkTableSizeWithParams:"

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

