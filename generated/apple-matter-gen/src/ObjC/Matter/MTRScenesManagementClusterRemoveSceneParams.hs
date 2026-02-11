{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterRemoveSceneParams@.
module ObjC.Matter.MTRScenesManagementClusterRemoveSceneParams
  ( MTRScenesManagementClusterRemoveSceneParams
  , IsMTRScenesManagementClusterRemoveSceneParams(..)
  , groupID
  , setGroupID
  , sceneID
  , setSceneID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupIDSelector
  , setGroupIDSelector
  , sceneIDSelector
  , setSceneIDSelector
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
groupID :: IsMTRScenesManagementClusterRemoveSceneParams mtrScenesManagementClusterRemoveSceneParams => mtrScenesManagementClusterRemoveSceneParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterRemoveSceneParams  =
    sendMsg mtrScenesManagementClusterRemoveSceneParams (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterRemoveSceneParams mtrScenesManagementClusterRemoveSceneParams, IsNSNumber value) => mtrScenesManagementClusterRemoveSceneParams -> value -> IO ()
setGroupID mtrScenesManagementClusterRemoveSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRemoveSceneParams (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterRemoveSceneParams mtrScenesManagementClusterRemoveSceneParams => mtrScenesManagementClusterRemoveSceneParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterRemoveSceneParams  =
    sendMsg mtrScenesManagementClusterRemoveSceneParams (mkSelector "sceneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterRemoveSceneParams mtrScenesManagementClusterRemoveSceneParams, IsNSNumber value) => mtrScenesManagementClusterRemoveSceneParams -> value -> IO ()
setSceneID mtrScenesManagementClusterRemoveSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRemoveSceneParams (mkSelector "setSceneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRScenesManagementClusterRemoveSceneParams mtrScenesManagementClusterRemoveSceneParams => mtrScenesManagementClusterRemoveSceneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrScenesManagementClusterRemoveSceneParams  =
    sendMsg mtrScenesManagementClusterRemoveSceneParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRScenesManagementClusterRemoveSceneParams mtrScenesManagementClusterRemoveSceneParams, IsNSNumber value) => mtrScenesManagementClusterRemoveSceneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrScenesManagementClusterRemoveSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRemoveSceneParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRScenesManagementClusterRemoveSceneParams mtrScenesManagementClusterRemoveSceneParams => mtrScenesManagementClusterRemoveSceneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrScenesManagementClusterRemoveSceneParams  =
    sendMsg mtrScenesManagementClusterRemoveSceneParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRScenesManagementClusterRemoveSceneParams mtrScenesManagementClusterRemoveSceneParams, IsNSNumber value) => mtrScenesManagementClusterRemoveSceneParams -> value -> IO ()
setServerSideProcessingTimeout mtrScenesManagementClusterRemoveSceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterRemoveSceneParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

