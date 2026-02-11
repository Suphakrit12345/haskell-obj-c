{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterSkipAreaResponseParams@.
module ObjC.Matter.MTRServiceAreaClusterSkipAreaResponseParams
  ( MTRServiceAreaClusterSkipAreaResponseParams
  , IsMTRServiceAreaClusterSkipAreaResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , statusText
  , setStatusText
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , statusTextSelector
  , setStatusTextSelector


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

-- | Initialize an MTRServiceAreaClusterSkipAreaResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrServiceAreaClusterSkipAreaResponseParams -> responseValue -> error_ -> IO (Id MTRServiceAreaClusterSkipAreaResponseParams)
initWithResponseValue_error mtrServiceAreaClusterSkipAreaResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrServiceAreaClusterSkipAreaResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams => mtrServiceAreaClusterSkipAreaResponseParams -> IO (Id NSNumber)
status mtrServiceAreaClusterSkipAreaResponseParams  =
    sendMsg mtrServiceAreaClusterSkipAreaResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams, IsNSNumber value) => mtrServiceAreaClusterSkipAreaResponseParams -> value -> IO ()
setStatus mtrServiceAreaClusterSkipAreaResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterSkipAreaResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusText@
statusText :: IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams => mtrServiceAreaClusterSkipAreaResponseParams -> IO (Id NSString)
statusText mtrServiceAreaClusterSkipAreaResponseParams  =
    sendMsg mtrServiceAreaClusterSkipAreaResponseParams (mkSelector "statusText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusText:@
setStatusText :: (IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams, IsNSString value) => mtrServiceAreaClusterSkipAreaResponseParams -> value -> IO ()
setStatusText mtrServiceAreaClusterSkipAreaResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterSkipAreaResponseParams (mkSelector "setStatusText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @statusText@
statusTextSelector :: Selector
statusTextSelector = mkSelector "statusText"

-- | @Selector@ for @setStatusText:@
setStatusTextSelector :: Selector
setStatusTextSelector = mkSelector "setStatusText:"

