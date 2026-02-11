{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterRecallSceneParams@.
module ObjC.Matter.MTRScenesManagementClusterRecallSceneParams
  ( MTRScenesManagementClusterRecallSceneParams
  , IsMTRScenesManagementClusterRecallSceneParams(..)
  , groupID
  , setGroupID
  , sceneID
  , setSceneID
  , transitionTime
  , setTransitionTime
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
groupID :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterRecallSceneParams  =
    sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setGroupID mtrScenesManagementClusterRecallSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterRecallSceneParams  =
    sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "sceneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setSceneID mtrScenesManagementClusterRecallSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "setSceneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
transitionTime mtrScenesManagementClusterRecallSceneParams  =
    sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setTransitionTime mtrScenesManagementClusterRecallSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrScenesManagementClusterRecallSceneParams  =
    sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrScenesManagementClusterRecallSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrScenesManagementClusterRecallSceneParams  =
    sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setServerSideProcessingTimeout mtrScenesManagementClusterRecallSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRecallSceneParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

