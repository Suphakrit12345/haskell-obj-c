{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSClientManagementClusterProvisionEndpointResponseParams@.
module ObjC.Matter.MTRTLSClientManagementClusterProvisionEndpointResponseParams
  ( MTRTLSClientManagementClusterProvisionEndpointResponseParams
  , IsMTRTLSClientManagementClusterProvisionEndpointResponseParams(..)
  , initWithResponseValue_error
  , endpointID
  , setEndpointID
  , initWithResponseValue_errorSelector
  , endpointIDSelector
  , setEndpointIDSelector


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

-- | Initialize an MTRTLSClientManagementClusterProvisionEndpointResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSClientManagementClusterProvisionEndpointResponseParams mtrtlsClientManagementClusterProvisionEndpointResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsClientManagementClusterProvisionEndpointResponseParams -> responseValue -> error_ -> IO (Id MTRTLSClientManagementClusterProvisionEndpointResponseParams)
initWithResponseValue_error mtrtlsClientManagementClusterProvisionEndpointResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrtlsClientManagementClusterProvisionEndpointResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- endpointID@
endpointID :: IsMTRTLSClientManagementClusterProvisionEndpointResponseParams mtrtlsClientManagementClusterProvisionEndpointResponseParams => mtrtlsClientManagementClusterProvisionEndpointResponseParams -> IO (Id NSNumber)
endpointID mtrtlsClientManagementClusterProvisionEndpointResponseParams  =
    sendMsg mtrtlsClientManagementClusterProvisionEndpointResponseParams (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointID:@
setEndpointID :: (IsMTRTLSClientManagementClusterProvisionEndpointResponseParams mtrtlsClientManagementClusterProvisionEndpointResponseParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointResponseParams -> value -> IO ()
setEndpointID mtrtlsClientManagementClusterProvisionEndpointResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterProvisionEndpointResponseParams (mkSelector "setEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector
setEndpointIDSelector = mkSelector "setEndpointID:"

