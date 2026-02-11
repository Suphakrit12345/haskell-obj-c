{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCCleanModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRRVCCleanModeClusterChangeToModeResponseParams
  ( MTRRVCCleanModeClusterChangeToModeResponseParams
  , IsMTRRVCCleanModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTRRVCCleanModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrrvcCleanModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRRVCCleanModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrrvcCleanModeClusterChangeToModeResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrrvcCleanModeClusterChangeToModeResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams => mtrrvcCleanModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrrvcCleanModeClusterChangeToModeResponseParams  =
    sendMsg mtrrvcCleanModeClusterChangeToModeResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrrvcCleanModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrrvcCleanModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcCleanModeClusterChangeToModeResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusText@
statusText :: IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams => mtrrvcCleanModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrrvcCleanModeClusterChangeToModeResponseParams  =
    sendMsg mtrrvcCleanModeClusterChangeToModeResponseParams (mkSelector "statusText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusText:@
setStatusText :: (IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams, IsNSString value) => mtrrvcCleanModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrrvcCleanModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcCleanModeClusterChangeToModeResponseParams (mkSelector "setStatusText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

