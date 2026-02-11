{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams
  ( MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterLookupClientCertificateResponseParams(..)
  , initWithResponseValue_error
  , ccdid
  , setCcdid
  , initWithResponseValue_errorSelector
  , ccdidSelector
  , setCcdidSelector


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

-- | Initialize an MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterLookupClientCertificateResponseParams mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterLookupClientCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- ccdid@
ccdid :: IsMTRTLSCertificateManagementClusterLookupClientCertificateResponseParams mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams => mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams -> IO (Id NSNumber)
ccdid mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams  =
    sendMsg mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams (mkSelector "ccdid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSCertificateManagementClusterLookupClientCertificateResponseParams mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams, IsNSNumber value) => mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams -> value -> IO ()
setCcdid mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterLookupClientCertificateResponseParams (mkSelector "setCcdid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector
setCcdidSelector = mkSelector "setCcdid:"

