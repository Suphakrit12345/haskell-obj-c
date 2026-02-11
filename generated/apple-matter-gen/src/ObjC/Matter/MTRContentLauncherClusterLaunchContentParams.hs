{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterLaunchContentParams@.
module ObjC.Matter.MTRContentLauncherClusterLaunchContentParams
  ( MTRContentLauncherClusterLaunchContentParams
  , IsMTRContentLauncherClusterLaunchContentParams(..)
  , search
  , setSearch
  , autoPlay
  , setAutoPlay
  , data_
  , setData
  , playbackPreferences
  , setPlaybackPreferences
  , useCurrentContext
  , setUseCurrentContext
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , searchSelector
  , setSearchSelector
  , autoPlaySelector
  , setAutoPlaySelector
  , dataSelector
  , setDataSelector
  , playbackPreferencesSelector
  , setPlaybackPreferencesSelector
  , useCurrentContextSelector
  , setUseCurrentContextSelector
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

-- | @- search@
search :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id MTRContentLauncherClusterContentSearchStruct)
search mtrContentLauncherClusterLaunchContentParams  =
    sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "search") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearch:@
setSearch :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsMTRContentLauncherClusterContentSearchStruct value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setSearch mtrContentLauncherClusterLaunchContentParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "setSearch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- autoPlay@
autoPlay :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSNumber)
autoPlay mtrContentLauncherClusterLaunchContentParams  =
    sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "autoPlay") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAutoPlay:@
setAutoPlay :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSNumber value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setAutoPlay mtrContentLauncherClusterLaunchContentParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "setAutoPlay:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- data@
data_ :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSString)
data_ mtrContentLauncherClusterLaunchContentParams  =
    sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSString value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setData mtrContentLauncherClusterLaunchContentParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- playbackPreferences@
playbackPreferences :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id MTRContentLauncherClusterPlaybackPreferencesStruct)
playbackPreferences mtrContentLauncherClusterLaunchContentParams  =
    sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "playbackPreferences") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaybackPreferences:@
setPlaybackPreferences :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsMTRContentLauncherClusterPlaybackPreferencesStruct value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setPlaybackPreferences mtrContentLauncherClusterLaunchContentParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "setPlaybackPreferences:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- useCurrentContext@
useCurrentContext :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSNumber)
useCurrentContext mtrContentLauncherClusterLaunchContentParams  =
    sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "useCurrentContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUseCurrentContext:@
setUseCurrentContext :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSNumber value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setUseCurrentContext mtrContentLauncherClusterLaunchContentParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "setUseCurrentContext:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentLauncherClusterLaunchContentParams  =
    sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSNumber value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentLauncherClusterLaunchContentParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentLauncherClusterLaunchContentParams  =
    sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSNumber value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentLauncherClusterLaunchContentParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLaunchContentParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @search@
searchSelector :: Selector
searchSelector = mkSelector "search"

-- | @Selector@ for @setSearch:@
setSearchSelector :: Selector
setSearchSelector = mkSelector "setSearch:"

-- | @Selector@ for @autoPlay@
autoPlaySelector :: Selector
autoPlaySelector = mkSelector "autoPlay"

-- | @Selector@ for @setAutoPlay:@
setAutoPlaySelector :: Selector
setAutoPlaySelector = mkSelector "setAutoPlay:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @playbackPreferences@
playbackPreferencesSelector :: Selector
playbackPreferencesSelector = mkSelector "playbackPreferences"

-- | @Selector@ for @setPlaybackPreferences:@
setPlaybackPreferencesSelector :: Selector
setPlaybackPreferencesSelector = mkSelector "setPlaybackPreferences:"

-- | @Selector@ for @useCurrentContext@
useCurrentContextSelector :: Selector
useCurrentContextSelector = mkSelector "useCurrentContext"

-- | @Selector@ for @setUseCurrentContext:@
setUseCurrentContextSelector :: Selector
setUseCurrentContextSelector = mkSelector "setUseCurrentContext:"

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

