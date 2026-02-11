{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterPayloadTestResponseParams@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterPayloadTestResponseParams
  ( MTRGeneralDiagnosticsClusterPayloadTestResponseParams
  , IsMTRGeneralDiagnosticsClusterPayloadTestResponseParams(..)
  , initWithResponseValue_error
  , payload
  , setPayload
  , initWithResponseValue_errorSelector
  , payloadSelector
  , setPayloadSelector


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

-- | Initialize an MTRGeneralDiagnosticsClusterPayloadTestResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGeneralDiagnosticsClusterPayloadTestResponseParams mtrGeneralDiagnosticsClusterPayloadTestResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGeneralDiagnosticsClusterPayloadTestResponseParams -> responseValue -> error_ -> IO (Id MTRGeneralDiagnosticsClusterPayloadTestResponseParams)
initWithResponseValue_error mtrGeneralDiagnosticsClusterPayloadTestResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrGeneralDiagnosticsClusterPayloadTestResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- payload@
payload :: IsMTRGeneralDiagnosticsClusterPayloadTestResponseParams mtrGeneralDiagnosticsClusterPayloadTestResponseParams => mtrGeneralDiagnosticsClusterPayloadTestResponseParams -> IO (Id NSData)
payload mtrGeneralDiagnosticsClusterPayloadTestResponseParams  =
    sendMsg mtrGeneralDiagnosticsClusterPayloadTestResponseParams (mkSelector "payload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPayload:@
setPayload :: (IsMTRGeneralDiagnosticsClusterPayloadTestResponseParams mtrGeneralDiagnosticsClusterPayloadTestResponseParams, IsNSData value) => mtrGeneralDiagnosticsClusterPayloadTestResponseParams -> value -> IO ()
setPayload mtrGeneralDiagnosticsClusterPayloadTestResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterPayloadTestResponseParams (mkSelector "setPayload:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @payload@
payloadSelector :: Selector
payloadSelector = mkSelector "payload"

-- | @Selector@ for @setPayload:@
setPayloadSelector :: Selector
setPayloadSelector = mkSelector "setPayload:"

