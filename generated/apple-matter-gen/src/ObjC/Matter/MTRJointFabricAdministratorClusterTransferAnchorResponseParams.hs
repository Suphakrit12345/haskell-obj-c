{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricAdministratorClusterTransferAnchorResponseParams@.
module ObjC.Matter.MTRJointFabricAdministratorClusterTransferAnchorResponseParams
  ( MTRJointFabricAdministratorClusterTransferAnchorResponseParams
  , IsMTRJointFabricAdministratorClusterTransferAnchorResponseParams(..)
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

-- | Initialize an MTRJointFabricAdministratorClusterTransferAnchorResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRJointFabricAdministratorClusterTransferAnchorResponseParams mtrJointFabricAdministratorClusterTransferAnchorResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrJointFabricAdministratorClusterTransferAnchorResponseParams -> responseValue -> error_ -> IO (Id MTRJointFabricAdministratorClusterTransferAnchorResponseParams)
initWithResponseValue_error mtrJointFabricAdministratorClusterTransferAnchorResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrJointFabricAdministratorClusterTransferAnchorResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- statusCode@
statusCode :: IsMTRJointFabricAdministratorClusterTransferAnchorResponseParams mtrJointFabricAdministratorClusterTransferAnchorResponseParams => mtrJointFabricAdministratorClusterTransferAnchorResponseParams -> IO (Id NSNumber)
statusCode mtrJointFabricAdministratorClusterTransferAnchorResponseParams  =
    sendMsg mtrJointFabricAdministratorClusterTransferAnchorResponseParams (mkSelector "statusCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusCode:@
setStatusCode :: (IsMTRJointFabricAdministratorClusterTransferAnchorResponseParams mtrJointFabricAdministratorClusterTransferAnchorResponseParams, IsNSNumber value) => mtrJointFabricAdministratorClusterTransferAnchorResponseParams -> value -> IO ()
setStatusCode mtrJointFabricAdministratorClusterTransferAnchorResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricAdministratorClusterTransferAnchorResponseParams (mkSelector "setStatusCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

