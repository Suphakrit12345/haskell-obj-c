{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster General Commissioning    This cluster is used to manage global aspects of the Commissioning flow.
--
-- Generated bindings for @MTRClusterGeneralCommissioning@.
module ObjC.Matter.MTRClusterGeneralCommissioning
  ( MTRClusterGeneralCommissioning
  , IsMTRClusterGeneralCommissioning(..)
  , armFailSafeWithParams_expectedValues_expectedValueInterval_completion
  , setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completion
  , commissioningCompleteWithParams_expectedValues_expectedValueInterval_completion
  , commissioningCompleteWithExpectedValues_expectedValueInterval_completion
  , setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeBreadcrumbWithParams
  , writeAttributeBreadcrumbWithValue_expectedValueInterval
  , writeAttributeBreadcrumbWithValue_expectedValueInterval_params
  , readAttributeBasicCommissioningInfoWithParams
  , readAttributeRegulatoryConfigWithParams
  , readAttributeLocationCapabilityWithParams
  , readAttributeSupportsConcurrentConnectionWithParams
  , readAttributeTCAcceptedVersionWithParams
  , readAttributeTCMinRequiredVersionWithParams
  , readAttributeTCAcknowledgementsWithParams
  , readAttributeTCAcknowledgementsRequiredWithParams
  , readAttributeTCUpdateDeadlineWithParams
  , readAttributeRecoveryIdentifierWithParams
  , readAttributeNetworkRecoveryReasonWithParams
  , readAttributeIsCommissioningWithoutPowerWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandler
  , setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandler
  , commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandler
  , commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , armFailSafeWithParams_expectedValues_expectedValueInterval_completionSelector
  , setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionSelector
  , commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionSelector
  , commissioningCompleteWithExpectedValues_expectedValueInterval_completionSelector
  , setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeBreadcrumbWithParamsSelector
  , writeAttributeBreadcrumbWithValue_expectedValueIntervalSelector
  , writeAttributeBreadcrumbWithValue_expectedValueInterval_paramsSelector
  , readAttributeBasicCommissioningInfoWithParamsSelector
  , readAttributeRegulatoryConfigWithParamsSelector
  , readAttributeLocationCapabilityWithParamsSelector
  , readAttributeSupportsConcurrentConnectionWithParamsSelector
  , readAttributeTCAcceptedVersionWithParamsSelector
  , readAttributeTCMinRequiredVersionWithParamsSelector
  , readAttributeTCAcknowledgementsWithParamsSelector
  , readAttributeTCAcknowledgementsRequiredWithParamsSelector
  , readAttributeTCUpdateDeadlineWithParamsSelector
  , readAttributeRecoveryIdentifierWithParamsSelector
  , readAttributeNetworkRecoveryReasonWithParamsSelector
  , readAttributeIsCommissioningWithoutPowerWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- armFailSafeWithParams:expectedValues:expectedValueInterval:completion:@
armFailSafeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterArmFailSafeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
armFailSafeWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralCommissioning (mkSelector "armFailSafeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completion:@
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralCommissioning (mkSelector "setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- commissioningCompleteWithParams:expectedValues:expectedValueInterval:completion:@
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterCommissioningCompleteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralCommissioning (mkSelector "commissioningCompleteWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- commissioningCompleteWithExpectedValues:expectedValueInterval:completion:@
commissioningCompleteWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
commissioningCompleteWithExpectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterGeneralCommissioning (mkSelector "commissioningCompleteWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setTCAcknowledgementsWithParams:expectedValues:expectedValueInterval:completion:@
setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralCommissioning (mkSelector "setTCAcknowledgementsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBreadcrumbWithParams:@
readAttributeBreadcrumbWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeBreadcrumbWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeBreadcrumbWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeBreadcrumbWithValue:expectedValueInterval:@
writeAttributeBreadcrumbWithValue_expectedValueInterval :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBreadcrumbWithValue_expectedValueInterval mtrClusterGeneralCommissioning  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterGeneralCommissioning (mkSelector "writeAttributeBreadcrumbWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeBreadcrumbWithValue:expectedValueInterval:params:@
writeAttributeBreadcrumbWithValue_expectedValueInterval_params :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterGeneralCommissioning -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBreadcrumbWithValue_expectedValueInterval_params mtrClusterGeneralCommissioning  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterGeneralCommissioning (mkSelector "writeAttributeBreadcrumbWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeBasicCommissioningInfoWithParams:@
readAttributeBasicCommissioningInfoWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeBasicCommissioningInfoWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeBasicCommissioningInfoWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRegulatoryConfigWithParams:@
readAttributeRegulatoryConfigWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeRegulatoryConfigWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeRegulatoryConfigWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLocationCapabilityWithParams:@
readAttributeLocationCapabilityWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeLocationCapabilityWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeLocationCapabilityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportsConcurrentConnectionWithParams:@
readAttributeSupportsConcurrentConnectionWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeSupportsConcurrentConnectionWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeSupportsConcurrentConnectionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTCAcceptedVersionWithParams:@
readAttributeTCAcceptedVersionWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCAcceptedVersionWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeTCAcceptedVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTCMinRequiredVersionWithParams:@
readAttributeTCMinRequiredVersionWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCMinRequiredVersionWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeTCMinRequiredVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTCAcknowledgementsWithParams:@
readAttributeTCAcknowledgementsWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCAcknowledgementsWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeTCAcknowledgementsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTCAcknowledgementsRequiredWithParams:@
readAttributeTCAcknowledgementsRequiredWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCAcknowledgementsRequiredWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeTCAcknowledgementsRequiredWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTCUpdateDeadlineWithParams:@
readAttributeTCUpdateDeadlineWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCUpdateDeadlineWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeTCUpdateDeadlineWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRecoveryIdentifierWithParams:@
readAttributeRecoveryIdentifierWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeRecoveryIdentifierWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeRecoveryIdentifierWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNetworkRecoveryReasonWithParams:@
readAttributeNetworkRecoveryReasonWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeNetworkRecoveryReasonWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeNetworkRecoveryReasonWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeIsCommissioningWithoutPowerWithParams:@
readAttributeIsCommissioningWithoutPowerWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeIsCommissioningWithoutPowerWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeIsCommissioningWithoutPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterGeneralCommissioning  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralCommissioning (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning => mtrClusterGeneralCommissioning -> IO (Id MTRClusterGeneralCommissioning)
init_ mtrClusterGeneralCommissioning  =
    sendMsg mtrClusterGeneralCommissioning (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterGeneralCommissioning)
new  =
  do
    cls' <- getRequiredClass "MTRClusterGeneralCommissioning"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRDevice device, IsNSObject queue) => mtrClusterGeneralCommissioning -> device -> CUShort -> queue -> IO (Id MTRClusterGeneralCommissioning)
initWithDevice_endpoint_queue mtrClusterGeneralCommissioning  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterGeneralCommissioning (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- armFailSafeWithParams:expectedValues:expectedValueInterval:completionHandler:@
armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterArmFailSafeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGeneralCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralCommissioning (mkSelector "armFailSafeWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completionHandler:@
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGeneralCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralCommissioning (mkSelector "setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- commissioningCompleteWithParams:expectedValues:expectedValueInterval:completionHandler:@
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterCommissioningCompleteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGeneralCommissioning  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralCommissioning (mkSelector "commissioningCompleteWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- commissioningCompleteWithExpectedValues:expectedValueInterval:completionHandler:@
commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandler mtrClusterGeneralCommissioning  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterGeneralCommissioning (mkSelector "commissioningCompleteWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterGeneralCommissioning -> device -> endpointID -> queue -> IO (Id MTRClusterGeneralCommissioning)
initWithDevice_endpointID_queue mtrClusterGeneralCommissioning  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterGeneralCommissioning (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @armFailSafeWithParams:expectedValues:expectedValueInterval:completion:@
armFailSafeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
armFailSafeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "armFailSafeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completion:@
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @commissioningCompleteWithParams:expectedValues:expectedValueInterval:completion:@
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "commissioningCompleteWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @commissioningCompleteWithExpectedValues:expectedValueInterval:completion:@
commissioningCompleteWithExpectedValues_expectedValueInterval_completionSelector :: Selector
commissioningCompleteWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "commissioningCompleteWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTCAcknowledgementsWithParams:expectedValues:expectedValueInterval:completion:@
setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTCAcknowledgementsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeBreadcrumbWithParams:@
readAttributeBreadcrumbWithParamsSelector :: Selector
readAttributeBreadcrumbWithParamsSelector = mkSelector "readAttributeBreadcrumbWithParams:"

-- | @Selector@ for @writeAttributeBreadcrumbWithValue:expectedValueInterval:@
writeAttributeBreadcrumbWithValue_expectedValueIntervalSelector :: Selector
writeAttributeBreadcrumbWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBreadcrumbWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBreadcrumbWithValue:expectedValueInterval:params:@
writeAttributeBreadcrumbWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeBreadcrumbWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBreadcrumbWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBasicCommissioningInfoWithParams:@
readAttributeBasicCommissioningInfoWithParamsSelector :: Selector
readAttributeBasicCommissioningInfoWithParamsSelector = mkSelector "readAttributeBasicCommissioningInfoWithParams:"

-- | @Selector@ for @readAttributeRegulatoryConfigWithParams:@
readAttributeRegulatoryConfigWithParamsSelector :: Selector
readAttributeRegulatoryConfigWithParamsSelector = mkSelector "readAttributeRegulatoryConfigWithParams:"

-- | @Selector@ for @readAttributeLocationCapabilityWithParams:@
readAttributeLocationCapabilityWithParamsSelector :: Selector
readAttributeLocationCapabilityWithParamsSelector = mkSelector "readAttributeLocationCapabilityWithParams:"

-- | @Selector@ for @readAttributeSupportsConcurrentConnectionWithParams:@
readAttributeSupportsConcurrentConnectionWithParamsSelector :: Selector
readAttributeSupportsConcurrentConnectionWithParamsSelector = mkSelector "readAttributeSupportsConcurrentConnectionWithParams:"

-- | @Selector@ for @readAttributeTCAcceptedVersionWithParams:@
readAttributeTCAcceptedVersionWithParamsSelector :: Selector
readAttributeTCAcceptedVersionWithParamsSelector = mkSelector "readAttributeTCAcceptedVersionWithParams:"

-- | @Selector@ for @readAttributeTCMinRequiredVersionWithParams:@
readAttributeTCMinRequiredVersionWithParamsSelector :: Selector
readAttributeTCMinRequiredVersionWithParamsSelector = mkSelector "readAttributeTCMinRequiredVersionWithParams:"

-- | @Selector@ for @readAttributeTCAcknowledgementsWithParams:@
readAttributeTCAcknowledgementsWithParamsSelector :: Selector
readAttributeTCAcknowledgementsWithParamsSelector = mkSelector "readAttributeTCAcknowledgementsWithParams:"

-- | @Selector@ for @readAttributeTCAcknowledgementsRequiredWithParams:@
readAttributeTCAcknowledgementsRequiredWithParamsSelector :: Selector
readAttributeTCAcknowledgementsRequiredWithParamsSelector = mkSelector "readAttributeTCAcknowledgementsRequiredWithParams:"

-- | @Selector@ for @readAttributeTCUpdateDeadlineWithParams:@
readAttributeTCUpdateDeadlineWithParamsSelector :: Selector
readAttributeTCUpdateDeadlineWithParamsSelector = mkSelector "readAttributeTCUpdateDeadlineWithParams:"

-- | @Selector@ for @readAttributeRecoveryIdentifierWithParams:@
readAttributeRecoveryIdentifierWithParamsSelector :: Selector
readAttributeRecoveryIdentifierWithParamsSelector = mkSelector "readAttributeRecoveryIdentifierWithParams:"

-- | @Selector@ for @readAttributeNetworkRecoveryReasonWithParams:@
readAttributeNetworkRecoveryReasonWithParamsSelector :: Selector
readAttributeNetworkRecoveryReasonWithParamsSelector = mkSelector "readAttributeNetworkRecoveryReasonWithParams:"

-- | @Selector@ for @readAttributeIsCommissioningWithoutPowerWithParams:@
readAttributeIsCommissioningWithoutPowerWithParamsSelector :: Selector
readAttributeIsCommissioningWithoutPowerWithParamsSelector = mkSelector "readAttributeIsCommissioningWithoutPowerWithParams:"

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

-- | @Selector@ for @armFailSafeWithParams:expectedValues:expectedValueInterval:completionHandler:@
armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "armFailSafeWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completionHandler:@
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @commissioningCompleteWithParams:expectedValues:expectedValueInterval:completionHandler:@
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "commissioningCompleteWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @commissioningCompleteWithExpectedValues:expectedValueInterval:completionHandler:@
commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "commissioningCompleteWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

