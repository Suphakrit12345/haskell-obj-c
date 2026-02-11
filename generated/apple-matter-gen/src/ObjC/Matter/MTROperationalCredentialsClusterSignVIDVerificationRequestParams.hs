{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterSignVIDVerificationRequestParams@.
module ObjC.Matter.MTROperationalCredentialsClusterSignVIDVerificationRequestParams
  ( MTROperationalCredentialsClusterSignVIDVerificationRequestParams
  , IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams(..)
  , fabricIndex
  , setFabricIndex
  , clientChallenge
  , setClientChallenge
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , fabricIndexSelector
  , setFabricIndexSelector
  , clientChallengeSelector
  , setClientChallengeSelector
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

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterSignVIDVerificationRequestParams  =
    sendMsg mtrOperationalCredentialsClusterSignVIDVerificationRequestParams (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterSignVIDVerificationRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSignVIDVerificationRequestParams (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clientChallenge@
clientChallenge :: IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> IO (Id NSData)
clientChallenge mtrOperationalCredentialsClusterSignVIDVerificationRequestParams  =
    sendMsg mtrOperationalCredentialsClusterSignVIDVerificationRequestParams (mkSelector "clientChallenge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClientChallenge:@
setClientChallenge :: (IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams, IsNSData value) => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> value -> IO ()
setClientChallenge mtrOperationalCredentialsClusterSignVIDVerificationRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSignVIDVerificationRequestParams (mkSelector "setClientChallenge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterSignVIDVerificationRequestParams  =
    sendMsg mtrOperationalCredentialsClusterSignVIDVerificationRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterSignVIDVerificationRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSignVIDVerificationRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterSignVIDVerificationRequestParams  =
    sendMsg mtrOperationalCredentialsClusterSignVIDVerificationRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams mtrOperationalCredentialsClusterSignVIDVerificationRequestParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterSignVIDVerificationRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterSignVIDVerificationRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

-- | @Selector@ for @clientChallenge@
clientChallengeSelector :: Selector
clientChallengeSelector = mkSelector "clientChallenge"

-- | @Selector@ for @setClientChallenge:@
setClientChallengeSelector :: Selector
setClientChallengeSelector = mkSelector "setClientChallenge:"

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

