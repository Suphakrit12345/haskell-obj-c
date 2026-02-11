{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Service Area    The Service Area cluster provides an interface for controlling the areas where a device should operate, and for querying the current area being serviced.
--
-- Generated bindings for @MTRClusterServiceArea@.
module ObjC.Matter.MTRClusterServiceArea
  ( MTRClusterServiceArea
  , IsMTRClusterServiceArea(..)
  , selectAreasWithParams_expectedValues_expectedValueInterval_completion
  , skipAreaWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSupportedAreasWithParams
  , readAttributeSupportedMapsWithParams
  , readAttributeSelectedAreasWithParams
  , readAttributeCurrentAreaWithParams
  , readAttributeEstimatedEndTimeWithParams
  , readAttributeProgressWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , selectAreasWithParams_expectedValues_expectedValueInterval_completionSelector
  , skipAreaWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeSupportedAreasWithParamsSelector
  , readAttributeSupportedMapsWithParamsSelector
  , readAttributeSelectedAreasWithParamsSelector
  , readAttributeCurrentAreaWithParamsSelector
  , readAttributeEstimatedEndTimeWithParamsSelector
  , readAttributeProgressWithParamsSelector
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

-- | @- selectAreasWithParams:expectedValues:expectedValueInterval:completion:@
selectAreasWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRServiceAreaClusterSelectAreasParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterServiceArea -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectAreasWithParams_expectedValues_expectedValueInterval_completion mtrClusterServiceArea  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterServiceArea (mkSelector "selectAreasWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- skipAreaWithParams:expectedValues:expectedValueInterval:completion:@
skipAreaWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRServiceAreaClusterSkipAreaParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterServiceArea -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipAreaWithParams_expectedValues_expectedValueInterval_completion mtrClusterServiceArea  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterServiceArea (mkSelector "skipAreaWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedAreasWithParams:@
readAttributeSupportedAreasWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeSupportedAreasWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeSupportedAreasWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedMapsWithParams:@
readAttributeSupportedMapsWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeSupportedMapsWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeSupportedMapsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSelectedAreasWithParams:@
readAttributeSelectedAreasWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeSelectedAreasWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeSelectedAreasWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentAreaWithParams:@
readAttributeCurrentAreaWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeCurrentAreaWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeCurrentAreaWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEstimatedEndTimeWithParams:@
readAttributeEstimatedEndTimeWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeEstimatedEndTimeWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeEstimatedEndTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeProgressWithParams:@
readAttributeProgressWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeProgressWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeProgressWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterServiceArea  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterServiceArea (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterServiceArea mtrClusterServiceArea => mtrClusterServiceArea -> IO (Id MTRClusterServiceArea)
init_ mtrClusterServiceArea  =
    sendMsg mtrClusterServiceArea (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterServiceArea)
new  =
  do
    cls' <- getRequiredClass "MTRClusterServiceArea"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterServiceArea -> device -> endpointID -> queue -> IO (Id MTRClusterServiceArea)
initWithDevice_endpointID_queue mtrClusterServiceArea  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterServiceArea (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectAreasWithParams:expectedValues:expectedValueInterval:completion:@
selectAreasWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
selectAreasWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "selectAreasWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @skipAreaWithParams:expectedValues:expectedValueInterval:completion:@
skipAreaWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
skipAreaWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "skipAreaWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSupportedAreasWithParams:@
readAttributeSupportedAreasWithParamsSelector :: Selector
readAttributeSupportedAreasWithParamsSelector = mkSelector "readAttributeSupportedAreasWithParams:"

-- | @Selector@ for @readAttributeSupportedMapsWithParams:@
readAttributeSupportedMapsWithParamsSelector :: Selector
readAttributeSupportedMapsWithParamsSelector = mkSelector "readAttributeSupportedMapsWithParams:"

-- | @Selector@ for @readAttributeSelectedAreasWithParams:@
readAttributeSelectedAreasWithParamsSelector :: Selector
readAttributeSelectedAreasWithParamsSelector = mkSelector "readAttributeSelectedAreasWithParams:"

-- | @Selector@ for @readAttributeCurrentAreaWithParams:@
readAttributeCurrentAreaWithParamsSelector :: Selector
readAttributeCurrentAreaWithParamsSelector = mkSelector "readAttributeCurrentAreaWithParams:"

-- | @Selector@ for @readAttributeEstimatedEndTimeWithParams:@
readAttributeEstimatedEndTimeWithParamsSelector :: Selector
readAttributeEstimatedEndTimeWithParamsSelector = mkSelector "readAttributeEstimatedEndTimeWithParams:"

-- | @Selector@ for @readAttributeProgressWithParams:@
readAttributeProgressWithParamsSelector :: Selector
readAttributeProgressWithParamsSelector = mkSelector "readAttributeProgressWithParams:"

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

