{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Target Navigator    This cluster provides an interface for UX navigation within a set of targets on a device or endpoint.
--
-- Generated bindings for @MTRClusterTargetNavigator@.
module ObjC.Matter.MTRClusterTargetNavigator
  ( MTRClusterTargetNavigator
  , IsMTRClusterTargetNavigator(..)
  , navigateTargetWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeTargetListWithParams
  , readAttributeCurrentTargetWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , navigateTargetWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , navigateTargetWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeTargetListWithParamsSelector
  , readAttributeCurrentTargetWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , navigateTargetWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- navigateTargetWithParams:expectedValues:expectedValueInterval:completion:@
navigateTargetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRTargetNavigatorClusterNavigateTargetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTargetNavigator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
navigateTargetWithParams_expectedValues_expectedValueInterval_completion mtrClusterTargetNavigator  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTargetNavigator (mkSelector "navigateTargetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTargetListWithParams:@
readAttributeTargetListWithParams :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRReadParams params) => mtrClusterTargetNavigator -> params -> IO (Id NSDictionary)
readAttributeTargetListWithParams mtrClusterTargetNavigator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTargetNavigator (mkSelector "readAttributeTargetListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentTargetWithParams:@
readAttributeCurrentTargetWithParams :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRReadParams params) => mtrClusterTargetNavigator -> params -> IO (Id NSDictionary)
readAttributeCurrentTargetWithParams mtrClusterTargetNavigator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTargetNavigator (mkSelector "readAttributeCurrentTargetWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRReadParams params) => mtrClusterTargetNavigator -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTargetNavigator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTargetNavigator (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRReadParams params) => mtrClusterTargetNavigator -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTargetNavigator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTargetNavigator (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRReadParams params) => mtrClusterTargetNavigator -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTargetNavigator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTargetNavigator (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRReadParams params) => mtrClusterTargetNavigator -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTargetNavigator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTargetNavigator (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRReadParams params) => mtrClusterTargetNavigator -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTargetNavigator  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTargetNavigator (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterTargetNavigator mtrClusterTargetNavigator => mtrClusterTargetNavigator -> IO (Id MTRClusterTargetNavigator)
init_ mtrClusterTargetNavigator  =
    sendMsg mtrClusterTargetNavigator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterTargetNavigator)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTargetNavigator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRDevice device, IsNSObject queue) => mtrClusterTargetNavigator -> device -> CUShort -> queue -> IO (Id MTRClusterTargetNavigator)
initWithDevice_endpoint_queue mtrClusterTargetNavigator  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterTargetNavigator (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- navigateTargetWithParams:expectedValues:expectedValueInterval:completionHandler:@
navigateTargetWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRTargetNavigatorClusterNavigateTargetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTargetNavigator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
navigateTargetWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterTargetNavigator  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTargetNavigator (mkSelector "navigateTargetWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTargetNavigator mtrClusterTargetNavigator, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTargetNavigator -> device -> endpointID -> queue -> IO (Id MTRClusterTargetNavigator)
initWithDevice_endpointID_queue mtrClusterTargetNavigator  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterTargetNavigator (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @navigateTargetWithParams:expectedValues:expectedValueInterval:completion:@
navigateTargetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
navigateTargetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "navigateTargetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeTargetListWithParams:@
readAttributeTargetListWithParamsSelector :: Selector
readAttributeTargetListWithParamsSelector = mkSelector "readAttributeTargetListWithParams:"

-- | @Selector@ for @readAttributeCurrentTargetWithParams:@
readAttributeCurrentTargetWithParamsSelector :: Selector
readAttributeCurrentTargetWithParamsSelector = mkSelector "readAttributeCurrentTargetWithParams:"

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

-- | @Selector@ for @navigateTargetWithParams:expectedValues:expectedValueInterval:completionHandler:@
navigateTargetWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
navigateTargetWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "navigateTargetWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

