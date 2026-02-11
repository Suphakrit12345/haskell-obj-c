{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams@.
module ObjC.Matter.MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams
  ( MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams
  , IsMTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams(..)
  , initWithResponseValue_error
  , errorCode
  , setErrorCode
  , initWithResponseValue_errorSelector
  , errorCodeSelector
  , setErrorCodeSelector


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

-- | Initialize an MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams -> responseValue -> error_ -> IO (Id MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams)
initWithResponseValue_error mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- errorCode@
errorCode :: IsMTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams => mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams -> IO (Id NSNumber)
errorCode mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams  =
    sendMsg mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams (mkSelector "errorCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorCode:@
setErrorCode :: (IsMTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams -> value -> IO ()
setErrorCode mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams (mkSelector "setErrorCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @errorCode@
errorCodeSelector :: Selector
errorCodeSelector = mkSelector "errorCode"

-- | @Selector@ for @setErrorCode:@
setErrorCodeSelector :: Selector
setErrorCodeSelector = mkSelector "setErrorCode:"

