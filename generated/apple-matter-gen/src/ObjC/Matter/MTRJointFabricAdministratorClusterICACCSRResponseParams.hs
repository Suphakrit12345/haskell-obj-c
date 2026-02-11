{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricAdministratorClusterICACCSRResponseParams@.
module ObjC.Matter.MTRJointFabricAdministratorClusterICACCSRResponseParams
  ( MTRJointFabricAdministratorClusterICACCSRResponseParams
  , IsMTRJointFabricAdministratorClusterICACCSRResponseParams(..)
  , initWithResponseValue_error
  , icaccsr
  , setIcaccsr
  , initWithResponseValue_errorSelector
  , icaccsrSelector
  , setIcaccsrSelector


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

-- | Initialize an MTRJointFabricAdministratorClusterICACCSRResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRJointFabricAdministratorClusterICACCSRResponseParams mtrJointFabricAdministratorClusterICACCSRResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrJointFabricAdministratorClusterICACCSRResponseParams -> responseValue -> error_ -> IO (Id MTRJointFabricAdministratorClusterICACCSRResponseParams)
initWithResponseValue_error mtrJointFabricAdministratorClusterICACCSRResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrJointFabricAdministratorClusterICACCSRResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- icaccsr@
icaccsr :: IsMTRJointFabricAdministratorClusterICACCSRResponseParams mtrJointFabricAdministratorClusterICACCSRResponseParams => mtrJointFabricAdministratorClusterICACCSRResponseParams -> IO (Id NSData)
icaccsr mtrJointFabricAdministratorClusterICACCSRResponseParams  =
    sendMsg mtrJointFabricAdministratorClusterICACCSRResponseParams (mkSelector "icaccsr") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIcaccsr:@
setIcaccsr :: (IsMTRJointFabricAdministratorClusterICACCSRResponseParams mtrJointFabricAdministratorClusterICACCSRResponseParams, IsNSData value) => mtrJointFabricAdministratorClusterICACCSRResponseParams -> value -> IO ()
setIcaccsr mtrJointFabricAdministratorClusterICACCSRResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricAdministratorClusterICACCSRResponseParams (mkSelector "setIcaccsr:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @icaccsr@
icaccsrSelector :: Selector
icaccsrSelector = mkSelector "icaccsr"

-- | @Selector@ for @setIcaccsr:@
setIcaccsrSelector :: Selector
setIcaccsrSelector = mkSelector "setIcaccsr:"

