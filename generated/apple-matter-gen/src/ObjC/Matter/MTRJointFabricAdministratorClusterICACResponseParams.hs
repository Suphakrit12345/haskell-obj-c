{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricAdministratorClusterICACResponseParams@.
module ObjC.Matter.MTRJointFabricAdministratorClusterICACResponseParams
  ( MTRJointFabricAdministratorClusterICACResponseParams
  , IsMTRJointFabricAdministratorClusterICACResponseParams(..)
  , initWithResponseValue_error
  , statusCode
  , setStatusCode
  , initWithResponseValue_errorSelector
  , statusCodeSelector
  , setStatusCodeSelector


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

-- | Initialize an MTRJointFabricAdministratorClusterICACResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRJointFabricAdministratorClusterICACResponseParams mtrJointFabricAdministratorClusterICACResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrJointFabricAdministratorClusterICACResponseParams -> responseValue -> error_ -> IO (Id MTRJointFabricAdministratorClusterICACResponseParams)
initWithResponseValue_error mtrJointFabricAdministratorClusterICACResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrJointFabricAdministratorClusterICACResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- statusCode@
statusCode :: IsMTRJointFabricAdministratorClusterICACResponseParams mtrJointFabricAdministratorClusterICACResponseParams => mtrJointFabricAdministratorClusterICACResponseParams -> IO (Id NSNumber)
statusCode mtrJointFabricAdministratorClusterICACResponseParams  =
    sendMsg mtrJointFabricAdministratorClusterICACResponseParams (mkSelector "statusCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusCode:@
setStatusCode :: (IsMTRJointFabricAdministratorClusterICACResponseParams mtrJointFabricAdministratorClusterICACResponseParams, IsNSNumber value) => mtrJointFabricAdministratorClusterICACResponseParams -> value -> IO ()
setStatusCode mtrJointFabricAdministratorClusterICACResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricAdministratorClusterICACResponseParams (mkSelector "setStatusCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector
setStatusCodeSelector = mkSelector "setStatusCode:"

