{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterFindRootCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterFindRootCertificateResponseParams
  ( MTRTLSCertificateManagementClusterFindRootCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterFindRootCertificateResponseParams(..)
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

-- | Initialize an MTRTLSCertificateManagementClusterFindRootCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterFindRootCertificateResponseParams mtrtlsCertificateManagementClusterFindRootCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterFindRootCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterFindRootCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterFindRootCertificateResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrtlsCertificateManagementClusterFindRootCertificateResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- certificateDetails@
certificateDetails :: IsMTRTLSCertificateManagementClusterFindRootCertificateResponseParams mtrtlsCertificateManagementClusterFindRootCertificateResponseParams => mtrtlsCertificateManagementClusterFindRootCertificateResponseParams -> IO (Id NSArray)
certificateDetails mtrtlsCertificateManagementClusterFindRootCertificateResponseParams  =
    sendMsg mtrtlsCertificateManagementClusterFindRootCertificateResponseParams (mkSelector "certificateDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCertificateDetails:@
setCertificateDetails :: (IsMTRTLSCertificateManagementClusterFindRootCertificateResponseParams mtrtlsCertificateManagementClusterFindRootCertificateResponseParams, IsNSArray value) => mtrtlsCertificateManagementClusterFindRootCertificateResponseParams -> value -> IO ()
setCertificateDetails mtrtlsCertificateManagementClusterFindRootCertificateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterFindRootCertificateResponseParams (mkSelector "setCertificateDetails:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

