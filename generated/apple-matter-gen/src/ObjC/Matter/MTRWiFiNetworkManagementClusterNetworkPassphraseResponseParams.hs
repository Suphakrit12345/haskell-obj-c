{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams@.
module ObjC.Matter.MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams
  ( MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams
  , IsMTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams(..)
  , initWithResponseValue_error
  , passphrase
  , setPassphrase
  , initWithResponseValue_errorSelector
  , passphraseSelector
  , setPassphraseSelector


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

-- | Initialize an MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams -> responseValue -> error_ -> IO (Id MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams)
initWithResponseValue_error mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- passphrase@
passphrase :: IsMTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams => mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams -> IO (Id NSData)
passphrase mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams  =
    sendMsg mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams (mkSelector "passphrase") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPassphrase:@
setPassphrase :: (IsMTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams, IsNSData value) => mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams -> value -> IO ()
setPassphrase mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams (mkSelector "setPassphrase:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @passphrase@
passphraseSelector :: Selector
passphraseSelector = mkSelector "passphrase"

-- | @Selector@ for @setPassphrase:@
setPassphraseSelector :: Selector
setPassphraseSelector = mkSelector "setPassphrase:"

