{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams@.
module ObjC.Matter.MTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams
  ( MTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams
  , IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams(..)
  , updateToken
  , setUpdateToken
  , softwareVersion
  , setSoftwareVersion
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , updateTokenSelector
  , setUpdateTokenSelector
  , softwareVersionSelector
  , setSoftwareVersionSelector
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
updateToken :: IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams => mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> IO (Id NSData)
updateToken mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams (mkSelector "updateToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUpdateToken:@
setUpdateToken :: (IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams, IsNSData value) => mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> value -> IO ()
setUpdateToken mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams (mkSelector "setUpdateToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- softwareVersion@
softwareVersion :: IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams => mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> IO (Id NSNumber)
softwareVersion mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams (mkSelector "softwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> value -> IO ()
setSoftwareVersion mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams (mkSelector "setSoftwareVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams => mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams => mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams  =
    sendMsg mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams, IsNSNumber value) => mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> value -> IO ()
setServerSideProcessingTimeout mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateToken@
updateTokenSelector :: Selector
updateTokenSelector = mkSelector "updateToken"

-- | @Selector@ for @setUpdateToken:@
setUpdateTokenSelector :: Selector
setUpdateTokenSelector = mkSelector "setUpdateToken:"

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

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

