{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterAddSceneResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterAddSceneResponseParams
  ( MTRScenesManagementClusterAddSceneResponseParams
  , IsMTRScenesManagementClusterAddSceneResponseParams(..)
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

-- | Initialize an MTRScenesManagementClusterAddSceneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterAddSceneResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterAddSceneResponseParams)
initWithResponseValue_error mtrScenesManagementClusterAddSceneResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrScenesManagementClusterAddSceneResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams => mtrScenesManagementClusterAddSceneResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterAddSceneResponseParams  =
    sendMsg mtrScenesManagementClusterAddSceneResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterAddSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams => mtrScenesManagementClusterAddSceneResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterAddSceneResponseParams  =
    sendMsg mtrScenesManagementClusterAddSceneResponseParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterAddSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneResponseParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams => mtrScenesManagementClusterAddSceneResponseParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterAddSceneResponseParams  =
    sendMsg mtrScenesManagementClusterAddSceneResponseParams (mkSelector "sceneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneResponseParams -> value -> IO ()
setSceneID mtrScenesManagementClusterAddSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneResponseParams (mkSelector "setSceneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

