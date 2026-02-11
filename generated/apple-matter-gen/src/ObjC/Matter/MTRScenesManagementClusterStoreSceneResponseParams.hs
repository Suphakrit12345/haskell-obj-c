{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterStoreSceneResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterStoreSceneResponseParams
  ( MTRScenesManagementClusterStoreSceneResponseParams
  , IsMTRScenesManagementClusterStoreSceneResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , sceneID
  , setSceneID
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , groupIDSelector
  , setGroupIDSelector
  , sceneIDSelector
  , setSceneIDSelector


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

-- | Initialize an MTRScenesManagementClusterStoreSceneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterStoreSceneResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterStoreSceneResponseParams)
initWithResponseValue_error mtrScenesManagementClusterStoreSceneResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrScenesManagementClusterStoreSceneResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams => mtrScenesManagementClusterStoreSceneResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterStoreSceneResponseParams  =
    sendMsg mtrScenesManagementClusterStoreSceneResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterStoreSceneResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterStoreSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterStoreSceneResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams => mtrScenesManagementClusterStoreSceneResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterStoreSceneResponseParams  =
    sendMsg mtrScenesManagementClusterStoreSceneResponseParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterStoreSceneResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterStoreSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterStoreSceneResponseParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams => mtrScenesManagementClusterStoreSceneResponseParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterStoreSceneResponseParams  =
    sendMsg mtrScenesManagementClusterStoreSceneResponseParams (mkSelector "sceneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterStoreSceneResponseParams -> value -> IO ()
setSceneID mtrScenesManagementClusterStoreSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterStoreSceneResponseParams (mkSelector "setSceneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @sceneID@
sceneIDSelector :: Selector
sceneIDSelector = mkSelector "sceneID"

-- | @Selector@ for @setSceneID:@
setSceneIDSelector :: Selector
setSceneIDSelector = mkSelector "setSceneID:"

