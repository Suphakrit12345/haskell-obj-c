{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Oven Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterOvenMode@.
module ObjC.Matter.MTRClusterOvenMode
  ( MTRClusterOvenMode
  , IsMTRClusterOvenMode(..)
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
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOvenMode mtrClusterOvenMode, IsMTROvenModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOvenMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterOvenMode  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOvenMode (mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterOvenMode mtrClusterOvenMode, IsMTRReadParams params) => mtrClusterOvenMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenMode (mkSelector "readAttributeSupportedModesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterOvenMode mtrClusterOvenMode, IsMTRReadParams params) => mtrClusterOvenMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenMode (mkSelector "readAttributeCurrentModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOvenMode mtrClusterOvenMode, IsMTRReadParams params) => mtrClusterOvenMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenMode (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOvenMode mtrClusterOvenMode, IsMTRReadParams params) => mtrClusterOvenMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenMode (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOvenMode mtrClusterOvenMode, IsMTRReadParams params) => mtrClusterOvenMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenMode (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOvenMode mtrClusterOvenMode, IsMTRReadParams params) => mtrClusterOvenMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenMode (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOvenMode mtrClusterOvenMode, IsMTRReadParams params) => mtrClusterOvenMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOvenMode  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOvenMode (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterOvenMode mtrClusterOvenMode => mtrClusterOvenMode -> IO (Id MTRClusterOvenMode)
init_ mtrClusterOvenMode  =
    sendMsg mtrClusterOvenMode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterOvenMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOvenMode"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOvenMode mtrClusterOvenMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOvenMode -> device -> endpointID -> queue -> IO (Id MTRClusterOvenMode)
initWithDevice_endpointID_queue mtrClusterOvenMode  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterOvenMode (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

