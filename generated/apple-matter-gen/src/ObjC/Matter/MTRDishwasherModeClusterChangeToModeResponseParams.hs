{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDishwasherModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRDishwasherModeClusterChangeToModeResponseParams
  ( MTRDishwasherModeClusterChangeToModeResponseParams
  , IsMTRDishwasherModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTRDishwasherModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDishwasherModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRDishwasherModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrDishwasherModeClusterChangeToModeResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDishwasherModeClusterChangeToModeResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams => mtrDishwasherModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrDishwasherModeClusterChangeToModeResponseParams  =
    sendMsg mtrDishwasherModeClusterChangeToModeResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrDishwasherModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrDishwasherModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherModeClusterChangeToModeResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusText@
statusText :: IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams => mtrDishwasherModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrDishwasherModeClusterChangeToModeResponseParams  =
    sendMsg mtrDishwasherModeClusterChangeToModeResponseParams (mkSelector "statusText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusText:@
setStatusText :: (IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams, IsNSString value) => mtrDishwasherModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrDishwasherModeClusterChangeToModeResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherModeClusterChangeToModeResponseParams (mkSelector "setStatusText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

