{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams@.
module ObjC.Matter.MTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams
  ( MTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams
  , IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams(..)
  , updateToken
  , setUpdateToken
  , newVersion
  , setNewVersion
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , updateTokenSelector
  , setUpdateTokenSelector
  , newVersionSelector
  , setNewVersionSelector
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

-- | @- updateToken@
updateToken :: IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams => mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> IO (Id NSData)
updateToken mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams (mkSelector "updateToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUpdateToken:@
setUpdateToken :: (IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams, IsNSData value) => mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> value -> IO ()
setUpdateToken mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams (mkSelector "setUpdateToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- newVersion@
newVersion :: IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams => mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> IO (Id NSNumber)
newVersion mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams (mkSelector "newVersion") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewVersion:@
setNewVersion :: (IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> value -> IO ()
setNewVersion mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams (mkSelector "setNewVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams => mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams => mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROtaSoftwareUpdateProviderClusterApplyUpdateRequestParams mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterApplyUpdateRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateToken@
updateTokenSelector :: Selector
updateTokenSelector = mkSelector "updateToken"

-- | @Selector@ for @setUpdateToken:@
setUpdateTokenSelector :: Selector
setUpdateTokenSelector = mkSelector "setUpdateToken:"

-- | @Selector@ for @newVersion@
newVersionSelector :: Selector
newVersionSelector = mkSelector "newVersion"

-- | @Selector@ for @setNewVersion:@
setNewVersionSelector :: Selector
setNewVersionSelector = mkSelector "setNewVersion:"

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

