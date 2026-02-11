{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Channel    This cluster provides an interface for controlling the current Channel on a device.
--
-- Generated bindings for @MTRClusterChannel@.
module ObjC.Matter.MTRClusterChannel
  ( MTRClusterChannel
  , IsMTRClusterChannel(..)
  , changeChannelWithParams_expectedValues_expectedValueInterval_completion
  , changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completion
  , skipChannelWithParams_expectedValues_expectedValueInterval_completion
  , getProgramGuideWithParams_expectedValues_expectedValueInterval_completion
  , getProgramGuideWithExpectedValues_expectedValueInterval_completion
  , recordProgramWithParams_expectedValues_expectedValueInterval_completion
  , cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeChannelListWithParams
  , readAttributeLineupWithParams
  , readAttributeCurrentChannelWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , changeChannelWithParams_expectedValues_expectedValueInterval_completionHandler
  , changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandler
  , skipChannelWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , changeChannelWithParams_expectedValues_expectedValueInterval_completionSelector
  , changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionSelector
  , skipChannelWithParams_expectedValues_expectedValueInterval_completionSelector
  , getProgramGuideWithParams_expectedValues_expectedValueInterval_completionSelector
  , getProgramGuideWithExpectedValues_expectedValueInterval_completionSelector
  , recordProgramWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeChannelListWithParamsSelector
  , readAttributeLineupWithParamsSelector
  , readAttributeCurrentChannelWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , changeChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , skipChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- changeChannelWithParams:expectedValues:expectedValueInterval:completion:@
changeChannelWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterChangeChannelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeChannelWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChannel (mkSelector "changeChannelWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completion:@
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterChangeChannelByNumberParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChannel (mkSelector "changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- skipChannelWithParams:expectedValues:expectedValueInterval:completion:@
skipChannelWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterSkipChannelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipChannelWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChannel (mkSelector "skipChannelWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getProgramGuideWithParams:expectedValues:expectedValueInterval:completion:@
getProgramGuideWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterGetProgramGuideParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getProgramGuideWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChannel (mkSelector "getProgramGuideWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getProgramGuideWithExpectedValues:expectedValueInterval:completion:@
getProgramGuideWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getProgramGuideWithExpectedValues_expectedValueInterval_completion mtrClusterChannel  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterChannel (mkSelector "getProgramGuideWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- recordProgramWithParams:expectedValues:expectedValueInterval:completion:@
recordProgramWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterRecordProgramParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
recordProgramWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChannel (mkSelector "recordProgramWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelRecordProgramWithParams:expectedValues:expectedValueInterval:completion:@
cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterCancelRecordProgramParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completion mtrClusterChannel  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChannel (mkSelector "cancelRecordProgramWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeChannelListWithParams:@
readAttributeChannelListWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeChannelListWithParams mtrClusterChannel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChannel (mkSelector "readAttributeChannelListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLineupWithParams:@
readAttributeLineupWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeLineupWithParams mtrClusterChannel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChannel (mkSelector "readAttributeLineupWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentChannelWithParams:@
readAttributeCurrentChannelWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeCurrentChannelWithParams mtrClusterChannel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChannel (mkSelector "readAttributeCurrentChannelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterChannel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChannel (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterChannel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChannel (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterChannel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChannel (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterChannel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChannel (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterChannel mtrClusterChannel, IsMTRReadParams params) => mtrClusterChannel -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterChannel  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChannel (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterChannel mtrClusterChannel => mtrClusterChannel -> IO (Id MTRClusterChannel)
init_ mtrClusterChannel  =
    sendMsg mtrClusterChannel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterChannel)
new  =
  do
    cls' <- getRequiredClass "MTRClusterChannel"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterChannel mtrClusterChannel, IsMTRDevice device, IsNSObject queue) => mtrClusterChannel -> device -> CUShort -> queue -> IO (Id MTRClusterChannel)
initWithDevice_endpoint_queue mtrClusterChannel  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterChannel (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- changeChannelWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeChannelWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterChangeChannelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeChannelWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterChannel  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChannel (mkSelector "changeChannelWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterChangeChannelByNumberParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterChannel  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChannel (mkSelector "changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- skipChannelWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipChannelWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterChannel mtrClusterChannel, IsMTRChannelClusterSkipChannelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChannel -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipChannelWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterChannel  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChannel (mkSelector "skipChannelWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterChannel mtrClusterChannel, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterChannel -> device -> endpointID -> queue -> IO (Id MTRClusterChannel)
initWithDevice_endpointID_queue mtrClusterChannel  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterChannel (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeChannelWithParams:expectedValues:expectedValueInterval:completion:@
changeChannelWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
changeChannelWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "changeChannelWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completion:@
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @skipChannelWithParams:expectedValues:expectedValueInterval:completion:@
skipChannelWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
skipChannelWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "skipChannelWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getProgramGuideWithParams:expectedValues:expectedValueInterval:completion:@
getProgramGuideWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
getProgramGuideWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getProgramGuideWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getProgramGuideWithExpectedValues:expectedValueInterval:completion:@
getProgramGuideWithExpectedValues_expectedValueInterval_completionSelector :: Selector
getProgramGuideWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getProgramGuideWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @recordProgramWithParams:expectedValues:expectedValueInterval:completion:@
recordProgramWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
recordProgramWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "recordProgramWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelRecordProgramWithParams:expectedValues:expectedValueInterval:completion:@
cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
cancelRecordProgramWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelRecordProgramWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeChannelListWithParams:@
readAttributeChannelListWithParamsSelector :: Selector
readAttributeChannelListWithParamsSelector = mkSelector "readAttributeChannelListWithParams:"

-- | @Selector@ for @readAttributeLineupWithParams:@
readAttributeLineupWithParamsSelector :: Selector
readAttributeLineupWithParamsSelector = mkSelector "readAttributeLineupWithParams:"

-- | @Selector@ for @readAttributeCurrentChannelWithParams:@
readAttributeCurrentChannelWithParamsSelector :: Selector
readAttributeCurrentChannelWithParamsSelector = mkSelector "readAttributeCurrentChannelWithParams:"

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

-- | @Selector@ for @changeChannelWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
changeChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "changeChannelWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
changeChannelByNumberWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "changeChannelByNumberWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @skipChannelWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
skipChannelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "skipChannelWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

