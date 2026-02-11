{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRWaterHeaterModeClusterChangeToModeResponseParams
  ( MTRWaterHeaterModeClusterChangeToModeResponseParams
  , IsMTRWaterHeaterModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTRWaterHeaterModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrWaterHeaterModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRWaterHeaterModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrWaterHeaterModeClusterChangeToModeResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrWaterHeaterModeClusterChangeToModeResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams => mtrWaterHeaterModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrWaterHeaterModeClusterChangeToModeResponseParams  =
    sendMsg mtrWaterHeaterModeClusterChangeToModeResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrWaterHeaterModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrWaterHeaterModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterModeClusterChangeToModeResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusText@
statusText :: IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams => mtrWaterHeaterModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrWaterHeaterModeClusterChangeToModeResponseParams  =
    sendMsg mtrWaterHeaterModeClusterChangeToModeResponseParams (mkSelector "statusText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusText:@
setStatusText :: (IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams, IsNSString value) => mtrWaterHeaterModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrWaterHeaterModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterModeClusterChangeToModeResponseParams (mkSelector "setStatusText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

