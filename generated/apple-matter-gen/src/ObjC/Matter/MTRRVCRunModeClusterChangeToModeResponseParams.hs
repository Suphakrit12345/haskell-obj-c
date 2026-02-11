{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCRunModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRRVCRunModeClusterChangeToModeResponseParams
  ( MTRRVCRunModeClusterChangeToModeResponseParams
  , IsMTRRVCRunModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTRRVCRunModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrrvcRunModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRRVCRunModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrrvcRunModeClusterChangeToModeResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrrvcRunModeClusterChangeToModeResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams => mtrrvcRunModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrrvcRunModeClusterChangeToModeResponseParams  =
    sendMsg mtrrvcRunModeClusterChangeToModeResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrrvcRunModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrrvcRunModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcRunModeClusterChangeToModeResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusText@
statusText :: IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams => mtrrvcRunModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrrvcRunModeClusterChangeToModeResponseParams  =
    sendMsg mtrrvcRunModeClusterChangeToModeResponseParams (mkSelector "statusText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusText:@
setStatusText :: (IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams, IsNSString value) => mtrrvcRunModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrrvcRunModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcRunModeClusterChangeToModeResponseParams (mkSelector "setStatusText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

