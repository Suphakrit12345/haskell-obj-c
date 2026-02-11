{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterViewSceneResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterViewSceneResponseParams
  ( MTRScenesManagementClusterViewSceneResponseParams
  , IsMTRScenesManagementClusterViewSceneResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , sceneID
  , setSceneID
  , transitionTime
  , setTransitionTime
  , sceneName
  , setSceneName
  , extensionFieldSetStructs
  , setExtensionFieldSetStructs
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , groupIDSelector
  , setGroupIDSelector
  , sceneIDSelector
  , setSceneIDSelector
  , transitionTimeSelector
  , setTransitionTimeSelector
  , sceneNameSelector
  , setSceneNameSelector
  , extensionFieldSetStructsSelector
  , setExtensionFieldSetStructsSelector


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

-- | Initialize an MTRScenesManagementClusterViewSceneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterViewSceneResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterViewSceneResponseParams)
initWithResponseValue_error mtrScenesManagementClusterViewSceneResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterViewSceneResponseParams  =
    sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterViewSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterViewSceneResponseParams  =
    sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterViewSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterViewSceneResponseParams  =
    sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "sceneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setSceneID mtrScenesManagementClusterViewSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "setSceneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSNumber)
transitionTime mtrScenesManagementClusterViewSceneResponseParams  =
    sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setTransitionTime mtrScenesManagementClusterViewSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneName@
sceneName :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSString)
sceneName mtrScenesManagementClusterViewSceneResponseParams  =
    sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "sceneName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneName:@
setSceneName :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSString value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setSceneName mtrScenesManagementClusterViewSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "setSceneName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- extensionFieldSetStructs@
extensionFieldSetStructs :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSArray)
extensionFieldSetStructs mtrScenesManagementClusterViewSceneResponseParams  =
    sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "extensionFieldSetStructs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtensionFieldSetStructs:@
setExtensionFieldSetStructs :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSArray value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setExtensionFieldSetStructs mtrScenesManagementClusterViewSceneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterViewSceneResponseParams (mkSelector "setExtensionFieldSetStructs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector
setTransitionTimeSelector = mkSelector "setTransitionTime:"

-- | @Selector@ for @sceneName@
sceneNameSelector :: Selector
sceneNameSelector = mkSelector "sceneName"

-- | @Selector@ for @setSceneName:@
setSceneNameSelector :: Selector
setSceneNameSelector = mkSelector "setSceneName:"

-- | @Selector@ for @extensionFieldSetStructs@
extensionFieldSetStructsSelector :: Selector
extensionFieldSetStructsSelector = mkSelector "extensionFieldSetStructs"

-- | @Selector@ for @setExtensionFieldSetStructs:@
setExtensionFieldSetStructsSelector :: Selector
setExtensionFieldSetStructsSelector = mkSelector "setExtensionFieldSetStructs:"

