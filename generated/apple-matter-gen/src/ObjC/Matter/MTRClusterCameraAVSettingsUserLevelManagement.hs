{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Camera AV Settings User Level Management    This cluster provides an interface into controls associated with the operation of a device that provides pan, tilt, and zoom functions, either mechanically, or against a digital image.
--
-- Generated bindings for @MTRClusterCameraAVSettingsUserLevelManagement@.
module ObjC.Matter.MTRClusterCameraAVSettingsUserLevelManagement
  ( MTRClusterCameraAVSettingsUserLevelManagement
  , IsMTRClusterCameraAVSettingsUserLevelManagement(..)
  , mptzSetPositionWithParams_expectedValues_expectedValueInterval_completion
  , mptzSetPositionWithExpectedValues_expectedValueInterval_completion
  , mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion
  , mptzRelativeMoveWithExpectedValues_expectedValueInterval_completion
  , mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completion
  , mptzSavePresetWithParams_expectedValues_expectedValueInterval_completion
  , mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completion
  , dptzSetViewportWithParams_expectedValues_expectedValueInterval_completion
  , dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMPTZPositionWithParams
  , readAttributeMaxPresetsWithParams
  , readAttributeMPTZPresetsWithParams
  , readAttributeDPTZStreamsWithParams
  , readAttributeZoomMaxWithParams
  , readAttributeTiltMinWithParams
  , readAttributeTiltMaxWithParams
  , readAttributePanMinWithParams
  , readAttributePanMaxWithParams
  , readAttributeMovementStateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , mptzSetPositionWithParams_expectedValues_expectedValueInterval_completionSelector
  , mptzSetPositionWithExpectedValues_expectedValueInterval_completionSelector
  , mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector
  , mptzRelativeMoveWithExpectedValues_expectedValueInterval_completionSelector
  , mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completionSelector
  , mptzSavePresetWithParams_expectedValues_expectedValueInterval_completionSelector
  , mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completionSelector
  , dptzSetViewportWithParams_expectedValues_expectedValueInterval_completionSelector
  , dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMPTZPositionWithParamsSelector
  , readAttributeMaxPresetsWithParamsSelector
  , readAttributeMPTZPresetsWithParamsSelector
  , readAttributeDPTZStreamsWithParamsSelector
  , readAttributeZoomMaxWithParamsSelector
  , readAttributeTiltMinWithParamsSelector
  , readAttributeTiltMaxWithParamsSelector
  , readAttributePanMinWithParamsSelector
  , readAttributePanMaxWithParamsSelector
  , readAttributeMovementStateWithParamsSelector
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

-- | @- MPTZSetPositionWithParams:expectedValues:expectedValueInterval:completion:@
mptzSetPositionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzSetPositionWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZSetPositionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- MPTZSetPositionWithExpectedValues:expectedValueInterval:completion:@
mptzSetPositionWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzSetPositionWithExpectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZSetPositionWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- MPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:@
mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- MPTZRelativeMoveWithExpectedValues:expectedValueInterval:completion:@
mptzRelativeMoveWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzRelativeMoveWithExpectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZRelativeMoveWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- MPTZMoveToPresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZMoveToPresetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- MPTZSavePresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzSavePresetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzSavePresetWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZSavePresetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- MPTZRemovePresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRemovePresetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "MPTZRemovePresetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- DPTZSetViewportWithParams:expectedValues:expectedValueInterval:completion:@
dptzSetViewportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
dptzSetViewportWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "DPTZSetViewportWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- DPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:@
dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "DPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMPTZPositionWithParams:@
readAttributeMPTZPositionWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeMPTZPositionWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeMPTZPositionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxPresetsWithParams:@
readAttributeMaxPresetsWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeMaxPresetsWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeMaxPresetsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMPTZPresetsWithParams:@
readAttributeMPTZPresetsWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeMPTZPresetsWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeMPTZPresetsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDPTZStreamsWithParams:@
readAttributeDPTZStreamsWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeDPTZStreamsWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeDPTZStreamsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeZoomMaxWithParams:@
readAttributeZoomMaxWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeZoomMaxWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeZoomMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTiltMinWithParams:@
readAttributeTiltMinWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeTiltMinWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeTiltMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTiltMaxWithParams:@
readAttributeTiltMaxWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeTiltMaxWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeTiltMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePanMinWithParams:@
readAttributePanMinWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributePanMinWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributePanMinWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePanMaxWithParams:@
readAttributePanMaxWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributePanMaxWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributePanMaxWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMovementStateWithParams:@
readAttributeMovementStateWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeMovementStateWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeMovementStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCameraAVSettingsUserLevelManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement => mtrClusterCameraAVSettingsUserLevelManagement -> IO (Id MTRClusterCameraAVSettingsUserLevelManagement)
init_ mtrClusterCameraAVSettingsUserLevelManagement  =
    sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterCameraAVSettingsUserLevelManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCameraAVSettingsUserLevelManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCameraAVSettingsUserLevelManagement -> device -> endpointID -> queue -> IO (Id MTRClusterCameraAVSettingsUserLevelManagement)
initWithDevice_endpointID_queue mtrClusterCameraAVSettingsUserLevelManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterCameraAVSettingsUserLevelManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @MPTZSetPositionWithParams:expectedValues:expectedValueInterval:completion:@
mptzSetPositionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
mptzSetPositionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZSetPositionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZSetPositionWithExpectedValues:expectedValueInterval:completion:@
mptzSetPositionWithExpectedValues_expectedValueInterval_completionSelector :: Selector
mptzSetPositionWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZSetPositionWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:@
mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZRelativeMoveWithExpectedValues:expectedValueInterval:completion:@
mptzRelativeMoveWithExpectedValues_expectedValueInterval_completionSelector :: Selector
mptzRelativeMoveWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZRelativeMoveWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZMoveToPresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZMoveToPresetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZSavePresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzSavePresetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
mptzSavePresetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZSavePresetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZRemovePresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZRemovePresetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @DPTZSetViewportWithParams:expectedValues:expectedValueInterval:completion:@
dptzSetViewportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
dptzSetViewportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "DPTZSetViewportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @DPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:@
dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "DPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMPTZPositionWithParams:@
readAttributeMPTZPositionWithParamsSelector :: Selector
readAttributeMPTZPositionWithParamsSelector = mkSelector "readAttributeMPTZPositionWithParams:"

-- | @Selector@ for @readAttributeMaxPresetsWithParams:@
readAttributeMaxPresetsWithParamsSelector :: Selector
readAttributeMaxPresetsWithParamsSelector = mkSelector "readAttributeMaxPresetsWithParams:"

-- | @Selector@ for @readAttributeMPTZPresetsWithParams:@
readAttributeMPTZPresetsWithParamsSelector :: Selector
readAttributeMPTZPresetsWithParamsSelector = mkSelector "readAttributeMPTZPresetsWithParams:"

-- | @Selector@ for @readAttributeDPTZStreamsWithParams:@
readAttributeDPTZStreamsWithParamsSelector :: Selector
readAttributeDPTZStreamsWithParamsSelector = mkSelector "readAttributeDPTZStreamsWithParams:"

-- | @Selector@ for @readAttributeZoomMaxWithParams:@
readAttributeZoomMaxWithParamsSelector :: Selector
readAttributeZoomMaxWithParamsSelector = mkSelector "readAttributeZoomMaxWithParams:"

-- | @Selector@ for @readAttributeTiltMinWithParams:@
readAttributeTiltMinWithParamsSelector :: Selector
readAttributeTiltMinWithParamsSelector = mkSelector "readAttributeTiltMinWithParams:"

-- | @Selector@ for @readAttributeTiltMaxWithParams:@
readAttributeTiltMaxWithParamsSelector :: Selector
readAttributeTiltMaxWithParamsSelector = mkSelector "readAttributeTiltMaxWithParams:"

-- | @Selector@ for @readAttributePanMinWithParams:@
readAttributePanMinWithParamsSelector :: Selector
readAttributePanMinWithParamsSelector = mkSelector "readAttributePanMinWithParams:"

-- | @Selector@ for @readAttributePanMaxWithParams:@
readAttributePanMaxWithParamsSelector :: Selector
readAttributePanMaxWithParamsSelector = mkSelector "readAttributePanMaxWithParams:"

-- | @Selector@ for @readAttributeMovementStateWithParams:@
readAttributeMovementStateWithParamsSelector :: Selector
readAttributeMovementStateWithParamsSelector = mkSelector "readAttributeMovementStateWithParams:"

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

