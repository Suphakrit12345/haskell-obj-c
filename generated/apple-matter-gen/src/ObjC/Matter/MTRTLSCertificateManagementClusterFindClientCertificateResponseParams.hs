{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterFindClientCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterFindClientCertificateResponseParams
  ( MTRTLSCertificateManagementClusterFindClientCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterFindClientCertificateResponseParams(..)
  , initWithResponseValue_error
  , certificateDetails
  , setCertificateDetails
  , initWithResponseValue_errorSelector
  , certificateDetailsSelector
  , setCertificateDetailsSelector


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

-- | Initialize an MTRTLSCertificateManagementClusterFindClientCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterFindClientCertificateResponseParams mtrtlsCertificateManagementClusterFindClientCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterFindClientCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterFindClientCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterFindClientCertificateResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrtlsCertificateManagementClusterFindClientCertificateResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- certificateDetails@
certificateDetails :: IsMTRTLSCertificateManagementClusterFindClientCertificateResponseParams mtrtlsCertificateManagementClusterFindClientCertificateResponseParams => mtrtlsCertificateManagementClusterFindClientCertificateResponseParams -> IO (Id NSArray)
certificateDetails mtrtlsCertificateManagementClusterFindClientCertificateResponseParams  =
    sendMsg mtrtlsCertificateManagementClusterFindClientCertificateResponseParams (mkSelector "certificateDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCertificateDetails:@
setCertificateDetails :: (IsMTRTLSCertificateManagementClusterFindClientCertificateResponseParams mtrtlsCertificateManagementClusterFindClientCertificateResponseParams, IsNSArray value) => mtrtlsCertificateManagementClusterFindClientCertificateResponseParams -> value -> IO ()
setCertificateDetails mtrtlsCertificateManagementClusterFindClientCertificateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterFindClientCertificateResponseParams (mkSelector "setCertificateDetails:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @certificateDetails@
certificateDetailsSelector :: Selector
certificateDetailsSelector = mkSelector "certificateDetails"

-- | @Selector@ for @setCertificateDetails:@
setCertificateDetailsSelector :: Selector
setCertificateDetailsSelector = mkSelector "setCertificateDetails:"

