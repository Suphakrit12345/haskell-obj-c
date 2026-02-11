{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Administrator Commissioning    Commands to trigger a Node to allow a new Administrator to commission it.
--
-- Generated bindings for @MTRClusterAdministratorCommissioning@.
module ObjC.Matter.MTRClusterAdministratorCommissioning
  ( MTRClusterAdministratorCommissioning
  , IsMTRClusterAdministratorCommissioning(..)
  , openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion
  , openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion
  , revokeCommissioningWithParams_expectedValues_expectedValueInterval_completion
  , revokeCommissioningWithExpectedValues_expectedValueInterval_completion
  , readAttributeWindowStatusWithParams
  , readAttributeAdminFabricIndexWithParams
  , readAttributeAdminVendorIdWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandler
  , openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandler
  , revokeCommissioningWithParams_expectedValues_expectedValueInterval_completionHandler
  , revokeCommissioningWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector
  , openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector
  , revokeCommissioningWithParams_expectedValues_expectedValueInterval_completionSelector
  , revokeCommissioningWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeWindowStatusWithParamsSelector
  , readAttributeAdminFabricIndexWithParamsSelector
  , readAttributeAdminVendorIdWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , revokeCommissioningWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , revokeCommissioningWithExpectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- openCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:@
openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAdministratorCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion mtrClusterAdministratorCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAdministratorCommissioning (mkSelector "openCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- openBasicCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:@
openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAdministratorCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion mtrClusterAdministratorCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAdministratorCommissioning (mkSelector "openBasicCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- revokeCommissioningWithParams:expectedValues:expectedValueInterval:completion:@
revokeCommissioningWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterRevokeCommissioningParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAdministratorCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
revokeCommissioningWithParams_expectedValues_expectedValueInterval_completion mtrClusterAdministratorCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAdministratorCommissioning (mkSelector "revokeCommissioningWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- revokeCommissioningWithExpectedValues:expectedValueInterval:completion:@
revokeCommissioningWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterAdministratorCommissioning -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
revokeCommissioningWithExpectedValues_expectedValueInterval_completion mtrClusterAdministratorCommissioning  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterAdministratorCommissioning (mkSelector "revokeCommissioningWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeWindowStatusWithParams:@
readAttributeWindowStatusWithParams :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRReadParams params) => mtrClusterAdministratorCommissioning -> params -> IO (Id NSDictionary)
readAttributeWindowStatusWithParams mtrClusterAdministratorCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAdministratorCommissioning (mkSelector "readAttributeWindowStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAdminFabricIndexWithParams:@
readAttributeAdminFabricIndexWithParams :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRReadParams params) => mtrClusterAdministratorCommissioning -> params -> IO (Id NSDictionary)
readAttributeAdminFabricIndexWithParams mtrClusterAdministratorCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAdministratorCommissioning (mkSelector "readAttributeAdminFabricIndexWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAdminVendorIdWithParams:@
readAttributeAdminVendorIdWithParams :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRReadParams params) => mtrClusterAdministratorCommissioning -> params -> IO (Id NSDictionary)
readAttributeAdminVendorIdWithParams mtrClusterAdministratorCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAdministratorCommissioning (mkSelector "readAttributeAdminVendorIdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRReadParams params) => mtrClusterAdministratorCommissioning -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAdministratorCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAdministratorCommissioning (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRReadParams params) => mtrClusterAdministratorCommissioning -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAdministratorCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAdministratorCommissioning (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRReadParams params) => mtrClusterAdministratorCommissioning -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAdministratorCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAdministratorCommissioning (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRReadParams params) => mtrClusterAdministratorCommissioning -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAdministratorCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAdministratorCommissioning (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRReadParams params) => mtrClusterAdministratorCommissioning -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAdministratorCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAdministratorCommissioning (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning => mtrClusterAdministratorCommissioning -> IO (Id MTRClusterAdministratorCommissioning)
init_ mtrClusterAdministratorCommissioning  =
    sendMsg mtrClusterAdministratorCommissioning (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterAdministratorCommissioning)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAdministratorCommissioning"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRDevice device, IsNSObject queue) => mtrClusterAdministratorCommissioning -> device -> CUShort -> queue -> IO (Id MTRClusterAdministratorCommissioning)
initWithDevice_endpoint_queue mtrClusterAdministratorCommissioning  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterAdministratorCommissioning (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- openCommissioningWindowWithParams:expectedValues:expectedValueInterval:completionHandler:@
openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAdministratorCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAdministratorCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAdministratorCommissioning (mkSelector "openCommissioningWindowWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- openBasicCommissioningWindowWithParams:expectedValues:expectedValueInterval:completionHandler:@
openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAdministratorCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAdministratorCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAdministratorCommissioning (mkSelector "openBasicCommissioningWindowWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- revokeCommissioningWithParams:expectedValues:expectedValueInterval:completionHandler:@
revokeCommissioningWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterRevokeCommissioningParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAdministratorCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
revokeCommissioningWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterAdministratorCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAdministratorCommissioning (mkSelector "revokeCommissioningWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- revokeCommissioningWithExpectedValues:expectedValueInterval:completionHandler:@
revokeCommissioningWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterAdministratorCommissioning -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
revokeCommissioningWithExpectedValues_expectedValueInterval_completionHandler mtrClusterAdministratorCommissioning  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterAdministratorCommissioning (mkSelector "revokeCommissioningWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAdministratorCommissioning mtrClusterAdministratorCommissioning, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAdministratorCommissioning -> device -> endpointID -> queue -> IO (Id MTRClusterAdministratorCommissioning)
initWithDevice_endpointID_queue mtrClusterAdministratorCommissioning  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterAdministratorCommissioning (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:@
openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "openCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @openBasicCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:@
openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "openBasicCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @revokeCommissioningWithParams:expectedValues:expectedValueInterval:completion:@
revokeCommissioningWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
revokeCommissioningWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "revokeCommissioningWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @revokeCommissioningWithExpectedValues:expectedValueInterval:completion:@
revokeCommissioningWithExpectedValues_expectedValueInterval_completionSelector :: Selector
revokeCommissioningWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "revokeCommissioningWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeWindowStatusWithParams:@
readAttributeWindowStatusWithParamsSelector :: Selector
readAttributeWindowStatusWithParamsSelector = mkSelector "readAttributeWindowStatusWithParams:"

-- | @Selector@ for @readAttributeAdminFabricIndexWithParams:@
readAttributeAdminFabricIndexWithParamsSelector :: Selector
readAttributeAdminFabricIndexWithParamsSelector = mkSelector "readAttributeAdminFabricIndexWithParams:"

-- | @Selector@ for @readAttributeAdminVendorIdWithParams:@
readAttributeAdminVendorIdWithParamsSelector :: Selector
readAttributeAdminVendorIdWithParamsSelector = mkSelector "readAttributeAdminVendorIdWithParams:"

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

-- | @Selector@ for @openCommissioningWindowWithParams:expectedValues:expectedValueInterval:completionHandler:@
openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
openCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "openCommissioningWindowWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @openBasicCommissioningWindowWithParams:expectedValues:expectedValueInterval:completionHandler:@
openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
openBasicCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "openBasicCommissioningWindowWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @revokeCommissioningWithParams:expectedValues:expectedValueInterval:completionHandler:@
revokeCommissioningWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
revokeCommissioningWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "revokeCommissioningWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @revokeCommissioningWithExpectedValues:expectedValueInterval:completionHandler:@
revokeCommissioningWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
revokeCommissioningWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "revokeCommissioningWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

