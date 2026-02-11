{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterCopySceneParams@.
module ObjC.Matter.MTRScenesManagementClusterCopySceneParams
  ( MTRScenesManagementClusterCopySceneParams
  , IsMTRScenesManagementClusterCopySceneParams(..)
  , mode
  , setMode
  , groupIdentifierFrom
  , setGroupIdentifierFrom
  , sceneIdentifierFrom
  , setSceneIdentifierFrom
  , groupIdentifierTo
  , setGroupIdentifierTo
  , sceneIdentifierTo
  , setSceneIdentifierTo
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , modeSelector
  , setModeSelector
  , groupIdentifierFromSelector
  , setGroupIdentifierFromSelector
  , sceneIdentifierFromSelector
  , setSceneIdentifierFromSelector
  , groupIdentifierToSelector
  , setGroupIdentifierToSelector
  , sceneIdentifierToSelector
  , setSceneIdentifierToSelector
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

-- | @- mode@
mode :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
mode mtrScenesManagementClusterCopySceneParams  =
    sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setMode mtrScenesManagementClusterCopySceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupIdentifierFrom@
groupIdentifierFrom :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
groupIdentifierFrom mtrScenesManagementClusterCopySceneParams  =
    sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "groupIdentifierFrom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupIdentifierFrom:@
setGroupIdentifierFrom :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setGroupIdentifierFrom mtrScenesManagementClusterCopySceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "setGroupIdentifierFrom:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneIdentifierFrom@
sceneIdentifierFrom :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
sceneIdentifierFrom mtrScenesManagementClusterCopySceneParams  =
    sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "sceneIdentifierFrom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneIdentifierFrom:@
setSceneIdentifierFrom :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setSceneIdentifierFrom mtrScenesManagementClusterCopySceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "setSceneIdentifierFrom:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupIdentifierTo@
groupIdentifierTo :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
groupIdentifierTo mtrScenesManagementClusterCopySceneParams  =
    sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "groupIdentifierTo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupIdentifierTo:@
setGroupIdentifierTo :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setGroupIdentifierTo mtrScenesManagementClusterCopySceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "setGroupIdentifierTo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneIdentifierTo@
sceneIdentifierTo :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
sceneIdentifierTo mtrScenesManagementClusterCopySceneParams  =
    sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "sceneIdentifierTo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneIdentifierTo:@
setSceneIdentifierTo :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setSceneIdentifierTo mtrScenesManagementClusterCopySceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "setSceneIdentifierTo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrScenesManagementClusterCopySceneParams  =
    sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrScenesManagementClusterCopySceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrScenesManagementClusterCopySceneParams  =
    sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setServerSideProcessingTimeout mtrScenesManagementClusterCopySceneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterCopySceneParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @groupIdentifierFrom@
groupIdentifierFromSelector :: Selector
groupIdentifierFromSelector = mkSelector "groupIdentifierFrom"

-- | @Selector@ for @setGroupIdentifierFrom:@
setGroupIdentifierFromSelector :: Selector
setGroupIdentifierFromSelector = mkSelector "setGroupIdentifierFrom:"

-- | @Selector@ for @sceneIdentifierFrom@
sceneIdentifierFromSelector :: Selector
sceneIdentifierFromSelector = mkSelector "sceneIdentifierFrom"

-- | @Selector@ for @setSceneIdentifierFrom:@
setSceneIdentifierFromSelector :: Selector
setSceneIdentifierFromSelector = mkSelector "setSceneIdentifierFrom:"

-- | @Selector@ for @groupIdentifierTo@
groupIdentifierToSelector :: Selector
groupIdentifierToSelector = mkSelector "groupIdentifierTo"

-- | @Selector@ for @setGroupIdentifierTo:@
setGroupIdentifierToSelector :: Selector
setGroupIdentifierToSelector = mkSelector "setGroupIdentifierTo:"

-- | @Selector@ for @sceneIdentifierTo@
sceneIdentifierToSelector :: Selector
sceneIdentifierToSelector = mkSelector "sceneIdentifierTo"

-- | @Selector@ for @setSceneIdentifierTo:@
setSceneIdentifierToSelector :: Selector
setSceneIdentifierToSelector = mkSelector "setSceneIdentifierTo:"

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

