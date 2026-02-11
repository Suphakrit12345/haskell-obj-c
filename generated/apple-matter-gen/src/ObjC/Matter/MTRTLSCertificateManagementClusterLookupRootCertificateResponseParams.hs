{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams
  ( MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterLookupRootCertificateResponseParams(..)
  , initWithResponseValue_error
  , caid
  , setCaid
  , initWithResponseValue_errorSelector
  , caidSelector
  , setCaidSelector


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

-- | Initialize an MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterLookupRootCertificateResponseParams mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterLookupRootCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- caid@
caid :: IsMTRTLSCertificateManagementClusterLookupRootCertificateResponseParams mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams => mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams -> IO (Id NSNumber)
caid mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams  =
    sendMsg mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams (mkSelector "caid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaid:@
setCaid :: (IsMTRTLSCertificateManagementClusterLookupRootCertificateResponseParams mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams, IsNSNumber value) => mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams -> value -> IO ()
setCaid mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterLookupRootCertificateResponseParams (mkSelector "setCaid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @caid@
caidSelector :: Selector
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector
setCaidSelector = mkSelector "setCaid:"

