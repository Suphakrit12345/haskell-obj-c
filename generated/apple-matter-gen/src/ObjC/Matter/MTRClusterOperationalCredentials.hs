{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Operational Credentials    This cluster is used to add or remove Operational Credentials on a Commissionee or Node, as well as manage the associated Fabrics.
--
-- Generated bindings for @MTRClusterOperationalCredentials@.
module ObjC.Matter.MTRClusterOperationalCredentials
  ( MTRClusterOperationalCredentials
  , IsMTRClusterOperationalCredentials(..)
  , attestationRequestWithParams_expectedValues_expectedValueInterval_completion
  , certificateChainRequestWithParams_expectedValues_expectedValueInterval_completion
  , csrRequestWithParams_expectedValues_expectedValueInterval_completion
  , addNOCWithParams_expectedValues_expectedValueInterval_completion
  , updateNOCWithParams_expectedValues_expectedValueInterval_completion
  , updateFabricLabelWithParams_expectedValues_expectedValueInterval_completion
  , removeFabricWithParams_expectedValues_expectedValueInterval_completion
  , addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completion
  , setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completion
  , signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeNOCsWithParams
  , readAttributeFabricsWithParams
  , readAttributeSupportedFabricsWithParams
  , readAttributeCommissionedFabricsWithParams
  , readAttributeTrustedRootCertificatesWithParams
  , readAttributeCurrentFabricIndexWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , csrRequestWithParams_expectedValues_expectedValueInterval_completionHandler
  , addNOCWithParams_expectedValues_expectedValueInterval_completionHandler
  , updateNOCWithParams_expectedValues_expectedValueInterval_completionHandler
  , updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandler
  , removeFabricWithParams_expectedValues_expectedValueInterval_completionHandler
  , addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , attestationRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , csrRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , addNOCWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateNOCWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeFabricWithParams_expectedValues_expectedValueInterval_completionSelector
  , addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completionSelector
  , setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completionSelector
  , signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeNOCsWithParamsSelector
  , readAttributeFabricsWithParamsSelector
  , readAttributeSupportedFabricsWithParamsSelector
  , readAttributeCommissionedFabricsWithParamsSelector
  , readAttributeTrustedRootCertificatesWithParamsSelector
  , readAttributeCurrentFabricIndexWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , csrRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , addNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , updateNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , removeFabricWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- attestationRequestWithParams:expectedValues:expectedValueInterval:completion:@
attestationRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAttestationRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
attestationRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "attestationRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- certificateChainRequestWithParams:expectedValues:expectedValueInterval:completion:@
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterCertificateChainRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "certificateChainRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- CSRRequestWithParams:expectedValues:expectedValueInterval:completion:@
csrRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterCSRRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
csrRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "CSRRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addNOCWithParams:expectedValues:expectedValueInterval:completion:@
addNOCWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddNOCParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addNOCWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "addNOCWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- updateNOCWithParams:expectedValues:expectedValueInterval:completion:@
updateNOCWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateNOCParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateNOCWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "updateNOCWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- updateFabricLabelWithParams:expectedValues:expectedValueInterval:completion:@
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateFabricLabelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "updateFabricLabelWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeFabricWithParams:expectedValues:expectedValueInterval:completion:@
removeFabricWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterRemoveFabricParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeFabricWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "removeFabricWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setVIDVerificationStatementWithParams:expectedValues:expectedValueInterval:completion:@
setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "setVIDVerificationStatementWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setVIDVerificationStatementWithExpectedValues:expectedValueInterval:completion:@
setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterOperationalCredentials (mkSelector "setVIDVerificationStatementWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- signVIDVerificationRequestWithParams:expectedValues:expectedValueInterval:completion:@
signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "signVIDVerificationRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNOCsWithParams:@
readAttributeNOCsWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeNOCsWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeNOCsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFabricsWithParams:@
readAttributeFabricsWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeFabricsWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeFabricsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportedFabricsWithParams:@
readAttributeSupportedFabricsWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeSupportedFabricsWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeSupportedFabricsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCommissionedFabricsWithParams:@
readAttributeCommissionedFabricsWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeCommissionedFabricsWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeCommissionedFabricsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTrustedRootCertificatesWithParams:@
readAttributeTrustedRootCertificatesWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeTrustedRootCertificatesWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeTrustedRootCertificatesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentFabricIndexWithParams:@
readAttributeCurrentFabricIndexWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeCurrentFabricIndexWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeCurrentFabricIndexWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRReadParams params) => mtrClusterOperationalCredentials -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOperationalCredentials  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterOperationalCredentials (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials => mtrClusterOperationalCredentials -> IO (Id MTRClusterOperationalCredentials)
init_ mtrClusterOperationalCredentials  =
    sendMsg mtrClusterOperationalCredentials (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterOperationalCredentials)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOperationalCredentials"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRDevice device, IsNSObject queue) => mtrClusterOperationalCredentials -> device -> CUShort -> queue -> IO (Id MTRClusterOperationalCredentials)
initWithDevice_endpoint_queue mtrClusterOperationalCredentials  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterOperationalCredentials (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- attestationRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAttestationRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "attestationRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- certificateChainRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterCertificateChainRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "certificateChainRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- CSRRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
csrRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterCSRRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
csrRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "CSRRequestWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addNOCWithParams:expectedValues:expectedValueInterval:completionHandler:@
addNOCWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddNOCParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addNOCWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "addNOCWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- updateNOCWithParams:expectedValues:expectedValueInterval:completionHandler:@
updateNOCWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateNOCParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateNOCWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "updateNOCWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- updateFabricLabelWithParams:expectedValues:expectedValueInterval:completionHandler:@
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateFabricLabelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "updateFabricLabelWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeFabricWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeFabricWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterRemoveFabricParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeFabricWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "removeFabricWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completionHandler:@
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalCredentials -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterOperationalCredentials  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOperationalCredentials mtrClusterOperationalCredentials, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOperationalCredentials -> device -> endpointID -> queue -> IO (Id MTRClusterOperationalCredentials)
initWithDevice_endpointID_queue mtrClusterOperationalCredentials  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterOperationalCredentials (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attestationRequestWithParams:expectedValues:expectedValueInterval:completion:@
attestationRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
attestationRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "attestationRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @certificateChainRequestWithParams:expectedValues:expectedValueInterval:completion:@
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "certificateChainRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @CSRRequestWithParams:expectedValues:expectedValueInterval:completion:@
csrRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
csrRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "CSRRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addNOCWithParams:expectedValues:expectedValueInterval:completion:@
addNOCWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addNOCWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addNOCWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateNOCWithParams:expectedValues:expectedValueInterval:completion:@
updateNOCWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updateNOCWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateNOCWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateFabricLabelWithParams:expectedValues:expectedValueInterval:completion:@
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateFabricLabelWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeFabricWithParams:expectedValues:expectedValueInterval:completion:@
removeFabricWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeFabricWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeFabricWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setVIDVerificationStatementWithParams:expectedValues:expectedValueInterval:completion:@
setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setVIDVerificationStatementWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setVIDVerificationStatementWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setVIDVerificationStatementWithExpectedValues:expectedValueInterval:completion:@
setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completionSelector :: Selector
setVIDVerificationStatementWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "setVIDVerificationStatementWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @signVIDVerificationRequestWithParams:expectedValues:expectedValueInterval:completion:@
signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
signVIDVerificationRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "signVIDVerificationRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeNOCsWithParams:@
readAttributeNOCsWithParamsSelector :: Selector
readAttributeNOCsWithParamsSelector = mkSelector "readAttributeNOCsWithParams:"

-- | @Selector@ for @readAttributeFabricsWithParams:@
readAttributeFabricsWithParamsSelector :: Selector
readAttributeFabricsWithParamsSelector = mkSelector "readAttributeFabricsWithParams:"

-- | @Selector@ for @readAttributeSupportedFabricsWithParams:@
readAttributeSupportedFabricsWithParamsSelector :: Selector
readAttributeSupportedFabricsWithParamsSelector = mkSelector "readAttributeSupportedFabricsWithParams:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithParams:@
readAttributeCommissionedFabricsWithParamsSelector :: Selector
readAttributeCommissionedFabricsWithParamsSelector = mkSelector "readAttributeCommissionedFabricsWithParams:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithParams:@
readAttributeTrustedRootCertificatesWithParamsSelector :: Selector
readAttributeTrustedRootCertificatesWithParamsSelector = mkSelector "readAttributeTrustedRootCertificatesWithParams:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithParams:@
readAttributeCurrentFabricIndexWithParamsSelector :: Selector
readAttributeCurrentFabricIndexWithParamsSelector = mkSelector "readAttributeCurrentFabricIndexWithParams:"

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

-- | @Selector@ for @attestationRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
attestationRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "attestationRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @certificateChainRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
certificateChainRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "certificateChainRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @CSRRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
csrRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
csrRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "CSRRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @addNOCWithParams:expectedValues:expectedValueInterval:completionHandler:@
addNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
addNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "addNOCWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @updateNOCWithParams:expectedValues:expectedValueInterval:completionHandler:@
updateNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
updateNOCWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "updateNOCWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @updateFabricLabelWithParams:expectedValues:expectedValueInterval:completionHandler:@
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
updateFabricLabelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "updateFabricLabelWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @removeFabricWithParams:expectedValues:expectedValueInterval:completionHandler:@
removeFabricWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
removeFabricWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "removeFabricWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completionHandler:@
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
addTrustedRootCertificateWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "addTrustedRootCertificateWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

