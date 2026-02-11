{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterRemoveAllScenesResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterRemoveAllScenesResponseParams
  ( MTRScenesManagementClusterRemoveAllScenesResponseParams
  , IsMTRScenesManagementClusterRemoveAllScenesResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , groupIDSelector
  , setGroupIDSelector


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

-- | Initialize an MTRScenesManagementClusterRemoveAllScenesResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterRemoveAllScenesResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterRemoveAllScenesResponseParams)
initWithResponseValue_error mtrScenesManagementClusterRemoveAllScenesResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrScenesManagementClusterRemoveAllScenesResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams => mtrScenesManagementClusterRemoveAllScenesResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterRemoveAllScenesResponseParams  =
    sendMsg mtrScenesManagementClusterRemoveAllScenesResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams, IsNSNumber value) => mtrScenesManagementClusterRemoveAllScenesResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterRemoveAllScenesResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRemoveAllScenesResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams => mtrScenesManagementClusterRemoveAllScenesResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterRemoveAllScenesResponseParams  =
    sendMsg mtrScenesManagementClusterRemoveAllScenesResponseParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams, IsNSNumber value) => mtrScenesManagementClusterRemoveAllScenesResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterRemoveAllScenesResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRemoveAllScenesResponseParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

