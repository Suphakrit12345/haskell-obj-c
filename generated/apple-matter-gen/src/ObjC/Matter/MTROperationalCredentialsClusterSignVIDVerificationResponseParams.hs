{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterSignVIDVerificationResponseParams@.
module ObjC.Matter.MTROperationalCredentialsClusterSignVIDVerificationResponseParams
  ( MTROperationalCredentialsClusterSignVIDVerificationResponseParams
  , IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams(..)
  , initWithResponseValue_error
  , fabricIndex
  , setFabricIndex
  , fabricBindingVersion
  , setFabricBindingVersion
  , signature
  , setSignature
  , initWithResponseValue_errorSelector
  , fabricIndexSelector
  , setFabricIndexSelector
  , fabricBindingVersionSelector
  , setFabricBindingVersionSelector
  , signatureSelector
  , setSignatureSelector


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

-- | Initialize an MTROperationalCredentialsClusterSignVIDVerificationResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> responseValue -> error_ -> IO (Id MTROperationalCredentialsClusterSignVIDVerificationResponseParams)
initWithResponseValue_error mtrOperationalCredentialsClusterSignVIDVerificationResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrOperationalCredentialsClusterSignVIDVerificationResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterSignVIDVerificationResponseParams  =
    sendMsg mtrOperationalCredentialsClusterSignVIDVerificationResponseParams (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterSignVIDVerificationResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSignVIDVerificationResponseParams (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricBindingVersion@
fabricBindingVersion :: IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> IO (Id NSNumber)
fabricBindingVersion mtrOperationalCredentialsClusterSignVIDVerificationResponseParams  =
    sendMsg mtrOperationalCredentialsClusterSignVIDVerificationResponseParams (mkSelector "fabricBindingVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricBindingVersion:@
setFabricBindingVersion :: (IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> value -> IO ()
setFabricBindingVersion mtrOperationalCredentialsClusterSignVIDVerificationResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSignVIDVerificationResponseParams (mkSelector "setFabricBindingVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- signature@
signature :: IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> IO (Id NSData)
signature mtrOperationalCredentialsClusterSignVIDVerificationResponseParams  =
    sendMsg mtrOperationalCredentialsClusterSignVIDVerificationResponseParams (mkSelector "signature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSignature:@
setSignature :: (IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams, IsNSData value) => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> value -> IO ()
setSignature mtrOperationalCredentialsClusterSignVIDVerificationResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSignVIDVerificationResponseParams (mkSelector "setSignature:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

-- | @Selector@ for @fabricBindingVersion@
fabricBindingVersionSelector :: Selector
fabricBindingVersionSelector = mkSelector "fabricBindingVersion"

-- | @Selector@ for @setFabricBindingVersion:@
setFabricBindingVersionSelector :: Selector
setFabricBindingVersionSelector = mkSelector "setFabricBindingVersion:"

-- | @Selector@ for @signature@
signatureSelector :: Selector
signatureSelector = mkSelector "signature"

-- | @Selector@ for @setSignature:@
setSignatureSelector :: Selector
setSignatureSelector = mkSelector "setSignature:"

