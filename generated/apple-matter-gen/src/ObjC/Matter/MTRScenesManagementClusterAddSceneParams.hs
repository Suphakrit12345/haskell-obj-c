{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterAddSceneParams@.
module ObjC.Matter.MTRScenesManagementClusterAddSceneParams
  ( MTRScenesManagementClusterAddSceneParams
  , IsMTRScenesManagementClusterAddSceneParams(..)
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
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
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
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterAddSceneParams  =
    sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setGroupID mtrScenesManagementClusterAddSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterAddSceneParams  =
    sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "sceneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setSceneID mtrScenesManagementClusterAddSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "setSceneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
transitionTime mtrScenesManagementClusterAddSceneParams  =
    sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setTransitionTime mtrScenesManagementClusterAddSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneName@
sceneName :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSString)
sceneName mtrScenesManagementClusterAddSceneParams  =
    sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "sceneName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneName:@
setSceneName :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSString value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setSceneName mtrScenesManagementClusterAddSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "setSceneName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- extensionFieldSetStructs@
extensionFieldSetStructs :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSArray)
extensionFieldSetStructs mtrScenesManagementClusterAddSceneParams  =
    sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "extensionFieldSetStructs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtensionFieldSetStructs:@
setExtensionFieldSetStructs :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSArray value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setExtensionFieldSetStructs mtrScenesManagementClusterAddSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "setExtensionFieldSetStructs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrScenesManagementClusterAddSceneParams  =
    sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrScenesManagementClusterAddSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrScenesManagementClusterAddSceneParams  =
    sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setServerSideProcessingTimeout mtrScenesManagementClusterAddSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAddSceneParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

