{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster RVC Run Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterRVCRunMode@.
module ObjC.Matter.MTRClusterRVCRunMode
  ( MTRClusterRVCRunMode
  , IsMTRClusterRVCRunMode(..)
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
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRRVCRunModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCRunMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterRVCRunMode  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterRVCRunMode (mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterRVCRunMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCRunMode (mkSelector "readAttributeSupportedModesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterRVCRunMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCRunMode (mkSelector "readAttributeCurrentModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRVCRunMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCRunMode (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRVCRunMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCRunMode (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRVCRunMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCRunMode (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRVCRunMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCRunMode (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRVCRunMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterRVCRunMode (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterRVCRunMode mtrClusterRVCRunMode => mtrClusterRVCRunMode -> IO (Id MTRClusterRVCRunMode)
init_ mtrClusterRVCRunMode  =
    sendMsg mtrClusterRVCRunMode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterRVCRunMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRVCRunMode"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRVCRunMode -> device -> endpointID -> queue -> IO (Id MTRClusterRVCRunMode)
initWithDevice_endpointID_queue mtrClusterRVCRunMode  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterRVCRunMode (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

