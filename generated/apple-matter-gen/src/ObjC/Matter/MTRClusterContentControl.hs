{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Content Control    This cluster is used for managing the content control (including "parental control") settings on a media device such as a TV, or Set-top Box.
--
-- Generated bindings for @MTRClusterContentControl@.
module ObjC.Matter.MTRClusterContentControl
  ( MTRClusterContentControl
  , IsMTRClusterContentControl(..)
  , updatePINWithParams_expectedValues_expectedValueInterval_completion
  , resetPINWithParams_expectedValues_expectedValueInterval_completion
  , resetPINWithExpectedValues_expectedValueInterval_completion
  , enableWithParams_expectedValues_expectedValueInterval_completion
  , enableWithExpectedValues_expectedValueInterval_completion
  , disableWithParams_expectedValues_expectedValueInterval_completion
  , disableWithExpectedValues_expectedValueInterval_completion
  , addBonusTimeWithParams_expectedValues_expectedValueInterval_completion
  , addBonusTimeWithExpectedValues_expectedValueInterval_completion
  , setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completion
  , blockUnratedContentWithParams_expectedValues_expectedValueInterval_completion
  , blockUnratedContentWithExpectedValues_expectedValueInterval_completion
  , unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completion
  , unblockUnratedContentWithExpectedValues_expectedValueInterval_completion
  , setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completion
  , setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeEnabledWithParams
  , readAttributeOnDemandRatingsWithParams
  , readAttributeOnDemandRatingThresholdWithParams
  , readAttributeScheduledContentRatingsWithParams
  , readAttributeScheduledContentRatingThresholdWithParams
  , readAttributeScreenDailyTimeWithParams
  , readAttributeRemainingScreenTimeWithParams
  , readAttributeBlockUnratedWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , updatePINWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetPINWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetPINWithExpectedValues_expectedValueInterval_completionSelector
  , enableWithParams_expectedValues_expectedValueInterval_completionSelector
  , enableWithExpectedValues_expectedValueInterval_completionSelector
  , disableWithParams_expectedValues_expectedValueInterval_completionSelector
  , disableWithExpectedValues_expectedValueInterval_completionSelector
  , addBonusTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , addBonusTimeWithExpectedValues_expectedValueInterval_completionSelector
  , setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , blockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector
  , blockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector
  , unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector
  , unblockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector
  , setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector
  , setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeEnabledWithParamsSelector
  , readAttributeOnDemandRatingsWithParamsSelector
  , readAttributeOnDemandRatingThresholdWithParamsSelector
  , readAttributeScheduledContentRatingsWithParamsSelector
  , readAttributeScheduledContentRatingThresholdWithParamsSelector
  , readAttributeScreenDailyTimeWithParamsSelector
  , readAttributeRemainingScreenTimeWithParamsSelector
  , readAttributeBlockUnratedWithParamsSelector
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

-- | @- updatePINWithParams:expectedValues:expectedValueInterval:completion:@
updatePINWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterUpdatePINParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updatePINWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "updatePINWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetPINWithParams:expectedValues:expectedValueInterval:completion:@
resetPINWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterResetPINParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetPINWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "resetPINWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetPINWithExpectedValues:expectedValueInterval:completion:@
resetPINWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetPINWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterContentControl (mkSelector "resetPINWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enableWithParams:expectedValues:expectedValueInterval:completion:@
enableWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterEnableParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "enableWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enableWithExpectedValues:expectedValueInterval:completion:@
enableWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
enableWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterContentControl (mkSelector "enableWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- disableWithParams:expectedValues:expectedValueInterval:completion:@
disableWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterDisableParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
disableWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "disableWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- disableWithExpectedValues:expectedValueInterval:completion:@
disableWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
disableWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterContentControl (mkSelector "disableWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addBonusTimeWithParams:expectedValues:expectedValueInterval:completion:@
addBonusTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterAddBonusTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addBonusTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "addBonusTimeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addBonusTimeWithExpectedValues:expectedValueInterval:completion:@
addBonusTimeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
addBonusTimeWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterContentControl (mkSelector "addBonusTimeWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setScreenDailyTimeWithParams:expectedValues:expectedValueInterval:completion:@
setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterSetScreenDailyTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "setScreenDailyTimeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- blockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:@
blockUnratedContentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterBlockUnratedContentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
blockUnratedContentWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "blockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- blockUnratedContentWithExpectedValues:expectedValueInterval:completion:@
blockUnratedContentWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
blockUnratedContentWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterContentControl (mkSelector "blockUnratedContentWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- unblockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:@
unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterUnblockUnratedContentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "unblockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- unblockUnratedContentWithExpectedValues:expectedValueInterval:completion:@
unblockUnratedContentWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
unblockUnratedContentWithExpectedValues_expectedValueInterval_completion mtrClusterContentControl  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterContentControl (mkSelector "unblockUnratedContentWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setOnDemandRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:@
setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterSetOnDemandRatingThresholdParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "setOnDemandRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setScheduledContentRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:@
setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRContentControlClusterSetScheduledContentRatingThresholdParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterContentControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completion mtrClusterContentControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterContentControl (mkSelector "setScheduledContentRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEnabledWithParams:@
readAttributeEnabledWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeEnabledWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOnDemandRatingsWithParams:@
readAttributeOnDemandRatingsWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeOnDemandRatingsWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeOnDemandRatingsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOnDemandRatingThresholdWithParams:@
readAttributeOnDemandRatingThresholdWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeOnDemandRatingThresholdWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeOnDemandRatingThresholdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeScheduledContentRatingsWithParams:@
readAttributeScheduledContentRatingsWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeScheduledContentRatingsWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeScheduledContentRatingsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeScheduledContentRatingThresholdWithParams:@
readAttributeScheduledContentRatingThresholdWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeScheduledContentRatingThresholdWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeScheduledContentRatingThresholdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeScreenDailyTimeWithParams:@
readAttributeScreenDailyTimeWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeScreenDailyTimeWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeScreenDailyTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRemainingScreenTimeWithParams:@
readAttributeRemainingScreenTimeWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeRemainingScreenTimeWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeRemainingScreenTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBlockUnratedWithParams:@
readAttributeBlockUnratedWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeBlockUnratedWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeBlockUnratedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRReadParams params) => mtrClusterContentControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterContentControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterContentControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterContentControl mtrClusterContentControl => mtrClusterContentControl -> IO (Id MTRClusterContentControl)
init_ mtrClusterContentControl  =
    sendMsg mtrClusterContentControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterContentControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterContentControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterContentControl mtrClusterContentControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterContentControl -> device -> endpointID -> queue -> IO (Id MTRClusterContentControl)
initWithDevice_endpointID_queue mtrClusterContentControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterContentControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updatePINWithParams:expectedValues:expectedValueInterval:completion:@
updatePINWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updatePINWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updatePINWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetPINWithParams:expectedValues:expectedValueInterval:completion:@
resetPINWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resetPINWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetPINWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetPINWithExpectedValues:expectedValueInterval:completion:@
resetPINWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resetPINWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetPINWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableWithParams:expectedValues:expectedValueInterval:completion:@
enableWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enableWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableWithExpectedValues:expectedValueInterval:completion:@
enableWithExpectedValues_expectedValueInterval_completionSelector :: Selector
enableWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "enableWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @disableWithParams:expectedValues:expectedValueInterval:completion:@
disableWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
disableWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "disableWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @disableWithExpectedValues:expectedValueInterval:completion:@
disableWithExpectedValues_expectedValueInterval_completionSelector :: Selector
disableWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "disableWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addBonusTimeWithParams:expectedValues:expectedValueInterval:completion:@
addBonusTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addBonusTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addBonusTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addBonusTimeWithExpectedValues:expectedValueInterval:completion:@
addBonusTimeWithExpectedValues_expectedValueInterval_completionSelector :: Selector
addBonusTimeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "addBonusTimeWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setScreenDailyTimeWithParams:expectedValues:expectedValueInterval:completion:@
setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setScreenDailyTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setScreenDailyTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @blockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:@
blockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
blockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "blockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @blockUnratedContentWithExpectedValues:expectedValueInterval:completion:@
blockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector :: Selector
blockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "blockUnratedContentWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unblockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:@
unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
unblockUnratedContentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "unblockUnratedContentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unblockUnratedContentWithExpectedValues:expectedValueInterval:completion:@
unblockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector :: Selector
unblockUnratedContentWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "unblockUnratedContentWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setOnDemandRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:@
setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setOnDemandRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setOnDemandRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setScheduledContentRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:@
setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setScheduledContentRatingThresholdWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setScheduledContentRatingThresholdWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeEnabledWithParams:@
readAttributeEnabledWithParamsSelector :: Selector
readAttributeEnabledWithParamsSelector = mkSelector "readAttributeEnabledWithParams:"

-- | @Selector@ for @readAttributeOnDemandRatingsWithParams:@
readAttributeOnDemandRatingsWithParamsSelector :: Selector
readAttributeOnDemandRatingsWithParamsSelector = mkSelector "readAttributeOnDemandRatingsWithParams:"

-- | @Selector@ for @readAttributeOnDemandRatingThresholdWithParams:@
readAttributeOnDemandRatingThresholdWithParamsSelector :: Selector
readAttributeOnDemandRatingThresholdWithParamsSelector = mkSelector "readAttributeOnDemandRatingThresholdWithParams:"

-- | @Selector@ for @readAttributeScheduledContentRatingsWithParams:@
readAttributeScheduledContentRatingsWithParamsSelector :: Selector
readAttributeScheduledContentRatingsWithParamsSelector = mkSelector "readAttributeScheduledContentRatingsWithParams:"

-- | @Selector@ for @readAttributeScheduledContentRatingThresholdWithParams:@
readAttributeScheduledContentRatingThresholdWithParamsSelector :: Selector
readAttributeScheduledContentRatingThresholdWithParamsSelector = mkSelector "readAttributeScheduledContentRatingThresholdWithParams:"

-- | @Selector@ for @readAttributeScreenDailyTimeWithParams:@
readAttributeScreenDailyTimeWithParamsSelector :: Selector
readAttributeScreenDailyTimeWithParamsSelector = mkSelector "readAttributeScreenDailyTimeWithParams:"

-- | @Selector@ for @readAttributeRemainingScreenTimeWithParams:@
readAttributeRemainingScreenTimeWithParamsSelector :: Selector
readAttributeRemainingScreenTimeWithParamsSelector = mkSelector "readAttributeRemainingScreenTimeWithParams:"

-- | @Selector@ for @readAttributeBlockUnratedWithParams:@
readAttributeBlockUnratedWithParamsSelector :: Selector
readAttributeBlockUnratedWithParamsSelector = mkSelector "readAttributeBlockUnratedWithParams:"

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

