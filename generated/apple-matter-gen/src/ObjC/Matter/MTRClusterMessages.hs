{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Messages    This cluster provides an interface for passing messages to be presented by a device.
--
-- Generated bindings for @MTRClusterMessages@.
module ObjC.Matter.MTRClusterMessages
  ( MTRClusterMessages
  , IsMTRClusterMessages(..)
  , presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completion
  , cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMessagesWithParams
  , readAttributeActiveMessageIDsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMessagesWithParamsSelector
  , readAttributeActiveMessageIDsWithParamsSelector
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

-- | @- presentMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:@
presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMessages mtrClusterMessages, IsMTRMessagesClusterPresentMessagesRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMessages -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterMessages  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMessages (mkSelector "presentMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMessages mtrClusterMessages, IsMTRMessagesClusterCancelMessagesRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMessages -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterMessages  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMessages (mkSelector "cancelMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMessagesWithParams:@
readAttributeMessagesWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeMessagesWithParams mtrClusterMessages  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMessages (mkSelector "readAttributeMessagesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveMessageIDsWithParams:@
readAttributeActiveMessageIDsWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeActiveMessageIDsWithParams mtrClusterMessages  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMessages (mkSelector "readAttributeActiveMessageIDsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMessages  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMessages (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMessages  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMessages (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMessages  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMessages (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMessages  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMessages (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMessages mtrClusterMessages, IsMTRReadParams params) => mtrClusterMessages -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMessages  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMessages (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterMessages mtrClusterMessages => mtrClusterMessages -> IO (Id MTRClusterMessages)
init_ mtrClusterMessages  =
    sendMsg mtrClusterMessages (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterMessages)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMessages"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMessages mtrClusterMessages, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMessages -> device -> endpointID -> queue -> IO (Id MTRClusterMessages)
initWithDevice_endpointID_queue mtrClusterMessages  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterMessages (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:@
presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
presentMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "presentMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
cancelMessagesRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelMessagesRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMessagesWithParams:@
readAttributeMessagesWithParamsSelector :: Selector
readAttributeMessagesWithParamsSelector = mkSelector "readAttributeMessagesWithParams:"

-- | @Selector@ for @readAttributeActiveMessageIDsWithParams:@
readAttributeActiveMessageIDsWithParamsSelector :: Selector
readAttributeActiveMessageIDsWithParamsSelector = mkSelector "readAttributeActiveMessageIDsWithParams:"

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

