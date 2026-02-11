{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterLeaveGroupResponseParams@.
module ObjC.Matter.MTRGroupcastClusterLeaveGroupResponseParams
  ( MTRGroupcastClusterLeaveGroupResponseParams
  , IsMTRGroupcastClusterLeaveGroupResponseParams(..)
  , initWithResponseValue_error
  , groupID
  , setGroupID
  , endpoints
  , setEndpoints
  , listTooLarge
  , setListTooLarge
  , initWithResponseValue_errorSelector
  , groupIDSelector
  , setGroupIDSelector
  , endpointsSelector
  , setEndpointsSelector
  , listTooLargeSelector
  , setListTooLargeSelector


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

-- | Initialize an MTRGroupcastClusterLeaveGroupResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupcastClusterLeaveGroupResponseParams -> responseValue -> error_ -> IO (Id MTRGroupcastClusterLeaveGroupResponseParams)
initWithResponseValue_error mtrGroupcastClusterLeaveGroupResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrGroupcastClusterLeaveGroupResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- groupID@
groupID :: IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams => mtrGroupcastClusterLeaveGroupResponseParams -> IO (Id NSNumber)
groupID mtrGroupcastClusterLeaveGroupResponseParams  =
    sendMsg mtrGroupcastClusterLeaveGroupResponseParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams, IsNSNumber value) => mtrGroupcastClusterLeaveGroupResponseParams -> value -> IO ()
setGroupID mtrGroupcastClusterLeaveGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterLeaveGroupResponseParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoints@
endpoints :: IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams => mtrGroupcastClusterLeaveGroupResponseParams -> IO (Id NSArray)
endpoints mtrGroupcastClusterLeaveGroupResponseParams  =
    sendMsg mtrGroupcastClusterLeaveGroupResponseParams (mkSelector "endpoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoints:@
setEndpoints :: (IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams, IsNSArray value) => mtrGroupcastClusterLeaveGroupResponseParams -> value -> IO ()
setEndpoints mtrGroupcastClusterLeaveGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterLeaveGroupResponseParams (mkSelector "setEndpoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- listTooLarge@
listTooLarge :: IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams => mtrGroupcastClusterLeaveGroupResponseParams -> IO (Id NSNumber)
listTooLarge mtrGroupcastClusterLeaveGroupResponseParams  =
    sendMsg mtrGroupcastClusterLeaveGroupResponseParams (mkSelector "listTooLarge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListTooLarge:@
setListTooLarge :: (IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams, IsNSNumber value) => mtrGroupcastClusterLeaveGroupResponseParams -> value -> IO ()
setListTooLarge mtrGroupcastClusterLeaveGroupResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupcastClusterLeaveGroupResponseParams (mkSelector "setListTooLarge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector
setEndpointsSelector = mkSelector "setEndpoints:"

-- | @Selector@ for @listTooLarge@
listTooLargeSelector :: Selector
listTooLargeSelector = mkSelector "listTooLarge"

-- | @Selector@ for @setListTooLarge:@
setListTooLargeSelector :: Selector
setListTooLargeSelector = mkSelector "setListTooLarge:"

