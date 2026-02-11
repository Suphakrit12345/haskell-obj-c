{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams@.
module ObjC.Matter.MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams
  ( MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams
  , IsMTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams(..)
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

-- | Initialize an MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams -> responseValue -> error_ -> IO (Id MTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams)
initWithResponseValue_error mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- caid@
caid :: IsMTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams => mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams -> IO (Id NSNumber)
caid mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams  =
    sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams (mkSelector "caid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaid:@
setCaid :: (IsMTRTLSCertificateManagementClusterProvisionRootCertificateResponseParams mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams, IsNSNumber value) => mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams -> value -> IO ()
setCaid mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsCertificateManagementClusterProvisionRootCertificateResponseParams (mkSelector "setCaid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

