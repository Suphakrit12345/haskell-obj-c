{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSClientManagementClusterFindEndpointResponseParams@.
module ObjC.Matter.MTRTLSClientManagementClusterFindEndpointResponseParams
  ( MTRTLSClientManagementClusterFindEndpointResponseParams
  , IsMTRTLSClientManagementClusterFindEndpointResponseParams(..)
  , initWithResponseValue_error
  , endpoint
  , setEndpoint
  , initWithResponseValue_errorSelector
  , endpointSelector
  , setEndpointSelector


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

-- | Initialize an MTRTLSClientManagementClusterFindEndpointResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTLSClientManagementClusterFindEndpointResponseParams mtrtlsClientManagementClusterFindEndpointResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrtlsClientManagementClusterFindEndpointResponseParams -> responseValue -> error_ -> IO (Id MTRTLSClientManagementClusterFindEndpointResponseParams)
initWithResponseValue_error mtrtlsClientManagementClusterFindEndpointResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrtlsClientManagementClusterFindEndpointResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- endpoint@
endpoint :: IsMTRTLSClientManagementClusterFindEndpointResponseParams mtrtlsClientManagementClusterFindEndpointResponseParams => mtrtlsClientManagementClusterFindEndpointResponseParams -> IO (Id MTRTLSClientManagementClusterTLSEndpointStruct)
endpoint mtrtlsClientManagementClusterFindEndpointResponseParams  =
    sendMsg mtrtlsClientManagementClusterFindEndpointResponseParams (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRTLSClientManagementClusterFindEndpointResponseParams mtrtlsClientManagementClusterFindEndpointResponseParams, IsMTRTLSClientManagementClusterTLSEndpointStruct value) => mtrtlsClientManagementClusterFindEndpointResponseParams -> value -> IO ()
setEndpoint mtrtlsClientManagementClusterFindEndpointResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrtlsClientManagementClusterFindEndpointResponseParams (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

