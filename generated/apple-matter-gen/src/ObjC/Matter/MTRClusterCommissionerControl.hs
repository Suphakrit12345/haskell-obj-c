{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commissioner Control    Supports the ability for clients to request the commissioning of themselves or other nodes onto a fabric which the cluster server can commission onto.
--
-- Generated bindings for @MTRClusterCommissionerControl@.
module ObjC.Matter.MTRClusterCommissionerControl
  ( MTRClusterCommissionerControl
  , IsMTRClusterCommissionerControl(..)
  , requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completion
  , commissionNodeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSupportedDeviceCategoriesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completionSelector
  , commissionNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeSupportedDeviceCategoriesWithParamsSelector
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

-- | @- requestCommissioningApprovalWithParams:expectedValues:expectedValueInterval:completion:@
requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRCommissionerControlClusterRequestCommissioningApprovalParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommissionerControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommissionerControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCommissionerControl (mkSelector "requestCommissioningApprovalWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- commissionNodeWithParams:expectedValues:expectedValueInterval:completion:@
commissionNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRCommissionerControlClusterCommissionNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommissionerControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
commissionNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommissionerControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCommissionerControl (mkSelector "commissionNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedDeviceCategoriesWithParams:@
readAttributeSupportedDeviceCategoriesWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeSupportedDeviceCategoriesWithParams mtrClusterCommissionerControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommissionerControl (mkSelector "readAttributeSupportedDeviceCategoriesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCommissionerControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommissionerControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCommissionerControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommissionerControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCommissionerControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommissionerControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCommissionerControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommissionerControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCommissionerControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCommissionerControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterCommissionerControl mtrClusterCommissionerControl => mtrClusterCommissionerControl -> IO (Id MTRClusterCommissionerControl)
init_ mtrClusterCommissionerControl  =
    sendMsg mtrClusterCommissionerControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterCommissionerControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCommissionerControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCommissionerControl -> device -> endpointID -> queue -> IO (Id MTRClusterCommissionerControl)
initWithDevice_endpointID_queue mtrClusterCommissionerControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterCommissionerControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestCommissioningApprovalWithParams:expectedValues:expectedValueInterval:completion:@
requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "requestCommissioningApprovalWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @commissionNodeWithParams:expectedValues:expectedValueInterval:completion:@
commissionNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
commissionNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "commissionNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSupportedDeviceCategoriesWithParams:@
readAttributeSupportedDeviceCategoriesWithParamsSelector :: Selector
readAttributeSupportedDeviceCategoriesWithParamsSelector = mkSelector "readAttributeSupportedDeviceCategoriesWithParams:"

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

