{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Actions    This cluster provides a standardized way for a Node (typically a Bridge, but could be any Node) to expose action information.
--
-- Generated bindings for @MTRClusterActions@.
module ObjC.Matter.MTRClusterActions
  ( MTRClusterActions
  , IsMTRClusterActions(..)
  , instantActionWithParams_expectedValues_expectedValueInterval_completion
  , instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completion
  , startActionWithParams_expectedValues_expectedValueInterval_completion
  , startActionWithDurationWithParams_expectedValues_expectedValueInterval_completion
  , stopActionWithParams_expectedValues_expectedValueInterval_completion
  , pauseActionWithParams_expectedValues_expectedValueInterval_completion
  , pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completion
  , resumeActionWithParams_expectedValues_expectedValueInterval_completion
  , enableActionWithParams_expectedValues_expectedValueInterval_completion
  , enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completion
  , disableActionWithParams_expectedValues_expectedValueInterval_completion
  , disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeActionListWithParams
  , readAttributeEndpointListsWithParams
  , readAttributeSetupURLWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , instantActionWithParams_expectedValues_expectedValueInterval_completionHandler
  , instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completionHandler
  , startActionWithParams_expectedValues_expectedValueInterval_completionHandler
  , startActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopActionWithParams_expectedValues_expectedValueInterval_completionHandler
  , pauseActionWithParams_expectedValues_expectedValueInterval_completionHandler
  , pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler
  , resumeActionWithParams_expectedValues_expectedValueInterval_completionHandler
  , enableActionWithParams_expectedValues_expectedValueInterval_completionHandler
  , enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler
  , disableActionWithParams_expectedValues_expectedValueInterval_completionHandler
  , disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , instantActionWithParams_expectedValues_expectedValueInterval_completionSelector
  , instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completionSelector
  , startActionWithParams_expectedValues_expectedValueInterval_completionSelector
  , startActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopActionWithParams_expectedValues_expectedValueInterval_completionSelector
  , pauseActionWithParams_expectedValues_expectedValueInterval_completionSelector
  , pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector
  , resumeActionWithParams_expectedValues_expectedValueInterval_completionSelector
  , enableActionWithParams_expectedValues_expectedValueInterval_completionSelector
  , enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector
  , disableActionWithParams_expectedValues_expectedValueInterval_completionSelector
  , disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeActionListWithParamsSelector
  , readAttributeEndpointListsWithParamsSelector
  , readAttributeSetupURLWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , instantActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , startActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , startActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , pauseActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , resumeActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , enableActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , disableActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- instantActionWithParams:expectedValues:expectedValueInterval:completion:@
instantActionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterInstantActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
instantActionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "instantActionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- instantActionWithTransitionWithParams:expectedValues:expectedValueInterval:completion:@
instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterInstantActionWithTransitionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "instantActionWithTransitionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startActionWithParams:expectedValues:expectedValueInterval:completion:@
startActionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterStartActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startActionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "startActionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:@
startActionWithDurationWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterStartActionWithDurationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startActionWithDurationWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "startActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopActionWithParams:expectedValues:expectedValueInterval:completion:@
stopActionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterStopActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopActionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "stopActionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pauseActionWithParams:expectedValues:expectedValueInterval:completion:@
pauseActionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterPauseActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseActionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "pauseActionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pauseActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:@
pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterPauseActionWithDurationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "pauseActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resumeActionWithParams:expectedValues:expectedValueInterval:completion:@
resumeActionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterResumeActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeActionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "resumeActionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enableActionWithParams:expectedValues:expectedValueInterval:completion:@
enableActionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterEnableActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableActionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "enableActionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enableActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:@
enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterEnableActionWithDurationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "enableActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- disableActionWithParams:expectedValues:expectedValueInterval:completion:@
disableActionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterDisableActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
disableActionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "disableActionWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- disableActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:@
disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterDisableActionWithDurationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completion mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "disableActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActionListWithParams:@
readAttributeActionListWithParams :: (IsMTRClusterActions mtrClusterActions, IsMTRReadParams params) => mtrClusterActions -> params -> IO (Id NSDictionary)
readAttributeActionListWithParams mtrClusterActions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActions (mkSelector "readAttributeActionListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEndpointListsWithParams:@
readAttributeEndpointListsWithParams :: (IsMTRClusterActions mtrClusterActions, IsMTRReadParams params) => mtrClusterActions -> params -> IO (Id NSDictionary)
readAttributeEndpointListsWithParams mtrClusterActions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActions (mkSelector "readAttributeEndpointListsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSetupURLWithParams:@
readAttributeSetupURLWithParams :: (IsMTRClusterActions mtrClusterActions, IsMTRReadParams params) => mtrClusterActions -> params -> IO (Id NSDictionary)
readAttributeSetupURLWithParams mtrClusterActions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActions (mkSelector "readAttributeSetupURLWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterActions mtrClusterActions, IsMTRReadParams params) => mtrClusterActions -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterActions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActions (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterActions mtrClusterActions, IsMTRReadParams params) => mtrClusterActions -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterActions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActions (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterActions mtrClusterActions, IsMTRReadParams params) => mtrClusterActions -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterActions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActions (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterActions mtrClusterActions, IsMTRReadParams params) => mtrClusterActions -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterActions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActions (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterActions mtrClusterActions, IsMTRReadParams params) => mtrClusterActions -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterActions  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterActions (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterActions mtrClusterActions => mtrClusterActions -> IO (Id MTRClusterActions)
init_ mtrClusterActions  =
    sendMsg mtrClusterActions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterActions)
new  =
  do
    cls' <- getRequiredClass "MTRClusterActions"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterActions mtrClusterActions, IsMTRDevice device, IsNSObject queue) => mtrClusterActions -> device -> CUShort -> queue -> IO (Id MTRClusterActions)
initWithDevice_endpoint_queue mtrClusterActions  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterActions (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- instantActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
instantActionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterInstantActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
instantActionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "instantActionWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- instantActionWithTransitionWithParams:expectedValues:expectedValueInterval:completionHandler:@
instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterInstantActionWithTransitionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "instantActionWithTransitionWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- startActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
startActionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterStartActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startActionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "startActionWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- startActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:@
startActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterStartActionWithDurationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "startActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopActionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterStopActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopActionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "stopActionWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pauseActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
pauseActionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterPauseActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseActionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "pauseActionWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pauseActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:@
pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterPauseActionWithDurationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "pauseActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resumeActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
resumeActionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterResumeActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeActionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "resumeActionWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- enableActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
enableActionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterEnableActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableActionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "enableActionWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- enableActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:@
enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterEnableActionWithDurationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "enableActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- disableActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
disableActionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterDisableActionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
disableActionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "disableActionWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- disableActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:@
disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterActions mtrClusterActions, IsMTRActionsClusterDisableActionWithDurationParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActions -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterActions  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterActions (mkSelector "disableActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterActions mtrClusterActions, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterActions -> device -> endpointID -> queue -> IO (Id MTRClusterActions)
initWithDevice_endpointID_queue mtrClusterActions  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterActions (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instantActionWithParams:expectedValues:expectedValueInterval:completion:@
instantActionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
instantActionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "instantActionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @instantActionWithTransitionWithParams:expectedValues:expectedValueInterval:completion:@
instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "instantActionWithTransitionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startActionWithParams:expectedValues:expectedValueInterval:completion:@
startActionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
startActionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startActionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:@
startActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
startActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopActionWithParams:expectedValues:expectedValueInterval:completion:@
stopActionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stopActionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopActionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseActionWithParams:expectedValues:expectedValueInterval:completion:@
pauseActionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
pauseActionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseActionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:@
pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeActionWithParams:expectedValues:expectedValueInterval:completion:@
resumeActionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resumeActionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resumeActionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableActionWithParams:expectedValues:expectedValueInterval:completion:@
enableActionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enableActionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableActionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:@
enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @disableActionWithParams:expectedValues:expectedValueInterval:completion:@
disableActionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
disableActionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "disableActionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @disableActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:@
disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "disableActionWithDurationWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeActionListWithParams:@
readAttributeActionListWithParamsSelector :: Selector
readAttributeActionListWithParamsSelector = mkSelector "readAttributeActionListWithParams:"

-- | @Selector@ for @readAttributeEndpointListsWithParams:@
readAttributeEndpointListsWithParamsSelector :: Selector
readAttributeEndpointListsWithParamsSelector = mkSelector "readAttributeEndpointListsWithParams:"

-- | @Selector@ for @readAttributeSetupURLWithParams:@
readAttributeSetupURLWithParamsSelector :: Selector
readAttributeSetupURLWithParamsSelector = mkSelector "readAttributeSetupURLWithParams:"

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

-- | @Selector@ for @instantActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
instantActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
instantActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "instantActionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @instantActionWithTransitionWithParams:expectedValues:expectedValueInterval:completionHandler:@
instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
instantActionWithTransitionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "instantActionWithTransitionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @startActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
startActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
startActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "startActionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @startActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:@
startActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
startActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "startActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stopActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopActionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @pauseActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
pauseActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
pauseActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "pauseActionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @pauseActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:@
pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
pauseActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "pauseActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @resumeActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
resumeActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
resumeActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resumeActionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @enableActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
enableActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
enableActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "enableActionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @enableActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:@
enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
enableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "enableActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @disableActionWithParams:expectedValues:expectedValueInterval:completionHandler:@
disableActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
disableActionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "disableActionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @disableActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:@
disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
disableActionWithDurationWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "disableActionWithDurationWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

