{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterAddThermostatSuggestionResponseParams@.
module ObjC.Matter.MTRThermostatClusterAddThermostatSuggestionResponseParams
  ( MTRThermostatClusterAddThermostatSuggestionResponseParams
  , IsMTRThermostatClusterAddThermostatSuggestionResponseParams(..)
  , initWithResponseValue_error
  , uniqueID
  , setUniqueID
  , initWithResponseValue_errorSelector
  , uniqueIDSelector
  , setUniqueIDSelector


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

-- | Initialize an MTRThermostatClusterAddThermostatSuggestionResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThermostatClusterAddThermostatSuggestionResponseParams mtrThermostatClusterAddThermostatSuggestionResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThermostatClusterAddThermostatSuggestionResponseParams -> responseValue -> error_ -> IO (Id MTRThermostatClusterAddThermostatSuggestionResponseParams)
initWithResponseValue_error mtrThermostatClusterAddThermostatSuggestionResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrThermostatClusterAddThermostatSuggestionResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- uniqueID@
uniqueID :: IsMTRThermostatClusterAddThermostatSuggestionResponseParams mtrThermostatClusterAddThermostatSuggestionResponseParams => mtrThermostatClusterAddThermostatSuggestionResponseParams -> IO (Id NSNumber)
uniqueID mtrThermostatClusterAddThermostatSuggestionResponseParams  =
    sendMsg mtrThermostatClusterAddThermostatSuggestionResponseParams (mkSelector "uniqueID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUniqueID:@
setUniqueID :: (IsMTRThermostatClusterAddThermostatSuggestionResponseParams mtrThermostatClusterAddThermostatSuggestionResponseParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionResponseParams -> value -> IO ()
setUniqueID mtrThermostatClusterAddThermostatSuggestionResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAddThermostatSuggestionResponseParams (mkSelector "setUniqueID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @setUniqueID:@
setUniqueIDSelector :: Selector
setUniqueIDSelector = mkSelector "setUniqueID:"

