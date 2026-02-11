{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Access Control    The Access Control Cluster exposes a data model view of a      Node's Access Control List (ACL), which codifies the rules used to manage      and enforce Access Control for the Node's endpoints and their associated      cluster instances.
--
-- Generated bindings for @MTRClusterAccessControl@.
module ObjC.Matter.MTRClusterAccessControl
  ( MTRClusterAccessControl
  , IsMTRClusterAccessControl(..)
  , reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeACLWithParams
  , writeAttributeACLWithValue_expectedValueInterval
  , writeAttributeACLWithValue_expectedValueInterval_params
  , readAttributeExtensionWithParams
  , writeAttributeExtensionWithValue_expectedValueInterval
  , writeAttributeExtensionWithValue_expectedValueInterval_params
  , readAttributeSubjectsPerAccessControlEntryWithParams
  , readAttributeTargetsPerAccessControlEntryWithParams
  , readAttributeAccessControlEntriesPerFabricWithParams
  , readAttributeCommissioningARLWithParams
  , readAttributeARLWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , readAttributeAclWithParams
  , writeAttributeAclWithValue_expectedValueInterval
  , writeAttributeAclWithValue_expectedValueInterval_params
  , initWithDevice_endpointID_queue
  , reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeACLWithParamsSelector
  , writeAttributeACLWithValue_expectedValueIntervalSelector
  , writeAttributeACLWithValue_expectedValueInterval_paramsSelector
  , readAttributeExtensionWithParamsSelector
  , writeAttributeExtensionWithValue_expectedValueIntervalSelector
  , writeAttributeExtensionWithValue_expectedValueInterval_paramsSelector
  , readAttributeSubjectsPerAccessControlEntryWithParamsSelector
  , readAttributeTargetsPerAccessControlEntryWithParamsSelector
  , readAttributeAccessControlEntriesPerFabricWithParamsSelector
  , readAttributeCommissioningARLWithParamsSelector
  , readAttributeARLWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , readAttributeAclWithParamsSelector
  , writeAttributeAclWithValue_expectedValueIntervalSelector
  , writeAttributeAclWithValue_expectedValueInterval_paramsSelector
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

-- | @- reviewFabricRestrictionsWithParams:expectedValues:expectedValueInterval:completion:@
reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRAccessControlClusterReviewFabricRestrictionsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccessControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completion mtrClusterAccessControl  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterAccessControl (mkSelector "reviewFabricRestrictionsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeACLWithParams:@
readAttributeACLWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeACLWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeACLWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeACLWithValue:expectedValueInterval:@
writeAttributeACLWithValue_expectedValueInterval :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeACLWithValue_expectedValueInterval mtrClusterAccessControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterAccessControl (mkSelector "writeAttributeACLWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeACLWithValue:expectedValueInterval:params:@
writeAttributeACLWithValue_expectedValueInterval_params :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeACLWithValue_expectedValueInterval_params mtrClusterAccessControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterAccessControl (mkSelector "writeAttributeACLWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeExtensionWithParams:@
readAttributeExtensionWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeExtensionWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeExtensionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeExtensionWithValue:expectedValueInterval:@
writeAttributeExtensionWithValue_expectedValueInterval :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeExtensionWithValue_expectedValueInterval mtrClusterAccessControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterAccessControl (mkSelector "writeAttributeExtensionWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeExtensionWithValue:expectedValueInterval:params:@
writeAttributeExtensionWithValue_expectedValueInterval_params :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeExtensionWithValue_expectedValueInterval_params mtrClusterAccessControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterAccessControl (mkSelector "writeAttributeExtensionWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSubjectsPerAccessControlEntryWithParams:@
readAttributeSubjectsPerAccessControlEntryWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeSubjectsPerAccessControlEntryWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeSubjectsPerAccessControlEntryWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTargetsPerAccessControlEntryWithParams:@
readAttributeTargetsPerAccessControlEntryWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeTargetsPerAccessControlEntryWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeTargetsPerAccessControlEntryWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAccessControlEntriesPerFabricWithParams:@
readAttributeAccessControlEntriesPerFabricWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeAccessControlEntriesPerFabricWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeAccessControlEntriesPerFabricWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCommissioningARLWithParams:@
readAttributeCommissioningARLWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeCommissioningARLWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeCommissioningARLWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeARLWithParams:@
readAttributeARLWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeARLWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeARLWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterAccessControl mtrClusterAccessControl => mtrClusterAccessControl -> IO (Id MTRClusterAccessControl)
init_ mtrClusterAccessControl  =
    sendMsg mtrClusterAccessControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterAccessControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAccessControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRDevice device, IsNSObject queue) => mtrClusterAccessControl -> device -> CUShort -> queue -> IO (Id MTRClusterAccessControl)
initWithDevice_endpoint_queue mtrClusterAccessControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterAccessControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeAclWithParams:@
readAttributeAclWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeAclWithParams mtrClusterAccessControl  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterAccessControl (mkSelector "readAttributeAclWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeAclWithValue:expectedValueInterval:@
writeAttributeAclWithValue_expectedValueInterval :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAclWithValue_expectedValueInterval mtrClusterAccessControl  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterAccessControl (mkSelector "writeAttributeAclWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeAclWithValue:expectedValueInterval:params:@
writeAttributeAclWithValue_expectedValueInterval_params :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAclWithValue_expectedValueInterval_params mtrClusterAccessControl  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterAccessControl (mkSelector "writeAttributeAclWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAccessControl -> device -> endpointID -> queue -> IO (Id MTRClusterAccessControl)
initWithDevice_endpointID_queue mtrClusterAccessControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterAccessControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reviewFabricRestrictionsWithParams:expectedValues:expectedValueInterval:completion:@
reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "reviewFabricRestrictionsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeACLWithParams:@
readAttributeACLWithParamsSelector :: Selector
readAttributeACLWithParamsSelector = mkSelector "readAttributeACLWithParams:"

-- | @Selector@ for @writeAttributeACLWithValue:expectedValueInterval:@
writeAttributeACLWithValue_expectedValueIntervalSelector :: Selector
writeAttributeACLWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeACLWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeACLWithValue:expectedValueInterval:params:@
writeAttributeACLWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeACLWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeACLWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeExtensionWithParams:@
readAttributeExtensionWithParamsSelector :: Selector
readAttributeExtensionWithParamsSelector = mkSelector "readAttributeExtensionWithParams:"

-- | @Selector@ for @writeAttributeExtensionWithValue:expectedValueInterval:@
writeAttributeExtensionWithValue_expectedValueIntervalSelector :: Selector
writeAttributeExtensionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeExtensionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeExtensionWithValue:expectedValueInterval:params:@
writeAttributeExtensionWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeExtensionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeExtensionWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithParams:@
readAttributeSubjectsPerAccessControlEntryWithParamsSelector :: Selector
readAttributeSubjectsPerAccessControlEntryWithParamsSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithParams:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithParams:@
readAttributeTargetsPerAccessControlEntryWithParamsSelector :: Selector
readAttributeTargetsPerAccessControlEntryWithParamsSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithParams:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithParams:@
readAttributeAccessControlEntriesPerFabricWithParamsSelector :: Selector
readAttributeAccessControlEntriesPerFabricWithParamsSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithParams:"

-- | @Selector@ for @readAttributeCommissioningARLWithParams:@
readAttributeCommissioningARLWithParamsSelector :: Selector
readAttributeCommissioningARLWithParamsSelector = mkSelector "readAttributeCommissioningARLWithParams:"

-- | @Selector@ for @readAttributeARLWithParams:@
readAttributeARLWithParamsSelector :: Selector
readAttributeARLWithParamsSelector = mkSelector "readAttributeARLWithParams:"

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

-- | @Selector@ for @readAttributeAclWithParams:@
readAttributeAclWithParamsSelector :: Selector
readAttributeAclWithParamsSelector = mkSelector "readAttributeAclWithParams:"

-- | @Selector@ for @writeAttributeAclWithValue:expectedValueInterval:@
writeAttributeAclWithValue_expectedValueIntervalSelector :: Selector
writeAttributeAclWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAclWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAclWithValue:expectedValueInterval:params:@
writeAttributeAclWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeAclWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAclWithValue:expectedValueInterval:params:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

