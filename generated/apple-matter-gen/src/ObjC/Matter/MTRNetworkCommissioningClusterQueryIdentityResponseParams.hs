{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterQueryIdentityResponseParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterQueryIdentityResponseParams
  ( MTRNetworkCommissioningClusterQueryIdentityResponseParams
  , IsMTRNetworkCommissioningClusterQueryIdentityResponseParams(..)
  , initWithResponseValue_error
  , identity
  , setIdentity
  , possessionSignature
  , setPossessionSignature
  , initWithResponseValue_errorSelector
  , identitySelector
  , setIdentitySelector
  , possessionSignatureSelector
  , setPossessionSignatureSelector


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

-- | Initialize an MTRNetworkCommissioningClusterQueryIdentityResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> responseValue -> error_ -> IO (Id MTRNetworkCommissioningClusterQueryIdentityResponseParams)
initWithResponseValue_error mtrNetworkCommissioningClusterQueryIdentityResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrNetworkCommissioningClusterQueryIdentityResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- identity@
identity :: IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> IO (Id NSData)
identity mtrNetworkCommissioningClusterQueryIdentityResponseParams  =
    sendMsg mtrNetworkCommissioningClusterQueryIdentityResponseParams (mkSelector "identity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentity:@
setIdentity :: (IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams, IsNSData value) => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> value -> IO ()
setIdentity mtrNetworkCommissioningClusterQueryIdentityResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterQueryIdentityResponseParams (mkSelector "setIdentity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- possessionSignature@
possessionSignature :: IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> IO (Id NSData)
possessionSignature mtrNetworkCommissioningClusterQueryIdentityResponseParams  =
    sendMsg mtrNetworkCommissioningClusterQueryIdentityResponseParams (mkSelector "possessionSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPossessionSignature:@
setPossessionSignature :: (IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams, IsNSData value) => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> value -> IO ()
setPossessionSignature mtrNetworkCommissioningClusterQueryIdentityResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterQueryIdentityResponseParams (mkSelector "setPossessionSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @identity@
identitySelector :: Selector
identitySelector = mkSelector "identity"

-- | @Selector@ for @setIdentity:@
setIdentitySelector :: Selector
setIdentitySelector = mkSelector "setIdentity:"

-- | @Selector@ for @possessionSignature@
possessionSignatureSelector :: Selector
possessionSignatureSelector = mkSelector "possessionSignature"

-- | @Selector@ for @setPossessionSignature:@
setPossessionSignatureSelector :: Selector
setPossessionSignatureSelector = mkSelector "setPossessionSignature:"

