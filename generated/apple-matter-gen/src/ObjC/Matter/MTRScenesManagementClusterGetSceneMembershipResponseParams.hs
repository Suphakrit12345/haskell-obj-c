{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterGetSceneMembershipResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterGetSceneMembershipResponseParams
  ( MTRScenesManagementClusterGetSceneMembershipResponseParams
  , IsMTRScenesManagementClusterGetSceneMembershipResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , capacity
  , setCapacity
  , groupID
  , setGroupID
  , sceneList
  , setSceneList
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , capacitySelector
  , setCapacitySelector
  , groupIDSelector
  , setGroupIDSelector
  , sceneListSelector
  , setSceneListSelector


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

-- | Initialize an MTRScenesManagementClusterGetSceneMembershipResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterGetSceneMembershipResponseParams)
initWithResponseValue_error mtrScenesManagementClusterGetSceneMembershipResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrScenesManagementClusterGetSceneMembershipResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams => mtrScenesManagementClusterGetSceneMembershipResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterGetSceneMembershipResponseParams  =
    sendMsg mtrScenesManagementClusterGetSceneMembershipResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSNumber value) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterGetSceneMembershipResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterGetSceneMembershipResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- capacity@
capacity :: IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams => mtrScenesManagementClusterGetSceneMembershipResponseParams -> IO (Id NSNumber)
capacity mtrScenesManagementClusterGetSceneMembershipResponseParams  =
    sendMsg mtrScenesManagementClusterGetSceneMembershipResponseParams (mkSelector "capacity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCapacity:@
setCapacity :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSNumber value) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> value -> IO ()
setCapacity mtrScenesManagementClusterGetSceneMembershipResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterGetSceneMembershipResponseParams (mkSelector "setCapacity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams => mtrScenesManagementClusterGetSceneMembershipResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterGetSceneMembershipResponseParams  =
    sendMsg mtrScenesManagementClusterGetSceneMembershipResponseParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSNumber value) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterGetSceneMembershipResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterGetSceneMembershipResponseParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneList@
sceneList :: IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams => mtrScenesManagementClusterGetSceneMembershipResponseParams -> IO (Id NSArray)
sceneList mtrScenesManagementClusterGetSceneMembershipResponseParams  =
    sendMsg mtrScenesManagementClusterGetSceneMembershipResponseParams (mkSelector "sceneList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneList:@
setSceneList :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSArray value) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> value -> IO ()
setSceneList mtrScenesManagementClusterGetSceneMembershipResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterGetSceneMembershipResponseParams (mkSelector "setSceneList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @capacity@
capacitySelector :: Selector
capacitySelector = mkSelector "capacity"

-- | @Selector@ for @setCapacity:@
setCapacitySelector :: Selector
setCapacitySelector = mkSelector "setCapacity:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @sceneList@
sceneListSelector :: Selector
sceneListSelector = mkSelector "sceneList"

-- | @Selector@ for @setSceneList:@
setSceneListSelector :: Selector
setSceneListSelector = mkSelector "setSceneList:"

