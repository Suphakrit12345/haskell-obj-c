{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccountLoginClusterLoginParams@.
module ObjC.Matter.MTRAccountLoginClusterLoginParams
  ( MTRAccountLoginClusterLoginParams
  , IsMTRAccountLoginClusterLoginParams(..)
  , tempAccountIdentifier
  , setTempAccountIdentifier
  , setupPIN
  , setSetupPIN
  , node
  , setNode
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , tempAccountIdentifierSelector
  , setTempAccountIdentifierSelector
  , setupPINSelector
  , setSetupPINSelector
  , nodeSelector
  , setNodeSelector
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

-- | @- tempAccountIdentifier@
tempAccountIdentifier :: IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams => mtrAccountLoginClusterLoginParams -> IO (Id NSString)
tempAccountIdentifier mtrAccountLoginClusterLoginParams  =
    sendMsg mtrAccountLoginClusterLoginParams (mkSelector "tempAccountIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTempAccountIdentifier:@
setTempAccountIdentifier :: (IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams, IsNSString value) => mtrAccountLoginClusterLoginParams -> value -> IO ()
setTempAccountIdentifier mtrAccountLoginClusterLoginParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccountLoginClusterLoginParams (mkSelector "setTempAccountIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- setupPIN@
setupPIN :: IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams => mtrAccountLoginClusterLoginParams -> IO (Id NSString)
setupPIN mtrAccountLoginClusterLoginParams  =
    sendMsg mtrAccountLoginClusterLoginParams (mkSelector "setupPIN") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSetupPIN:@
setSetupPIN :: (IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams, IsNSString value) => mtrAccountLoginClusterLoginParams -> value -> IO ()
setSetupPIN mtrAccountLoginClusterLoginParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccountLoginClusterLoginParams (mkSelector "setSetupPIN:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- node@
node :: IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams => mtrAccountLoginClusterLoginParams -> IO (Id NSNumber)
node mtrAccountLoginClusterLoginParams  =
    sendMsg mtrAccountLoginClusterLoginParams (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNode:@
setNode :: (IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams, IsNSNumber value) => mtrAccountLoginClusterLoginParams -> value -> IO ()
setNode mtrAccountLoginClusterLoginParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccountLoginClusterLoginParams (mkSelector "setNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams => mtrAccountLoginClusterLoginParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrAccountLoginClusterLoginParams  =
    sendMsg mtrAccountLoginClusterLoginParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams, IsNSNumber value) => mtrAccountLoginClusterLoginParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrAccountLoginClusterLoginParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccountLoginClusterLoginParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams => mtrAccountLoginClusterLoginParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrAccountLoginClusterLoginParams  =
    sendMsg mtrAccountLoginClusterLoginParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRAccountLoginClusterLoginParams mtrAccountLoginClusterLoginParams, IsNSNumber value) => mtrAccountLoginClusterLoginParams -> value -> IO ()
setServerSideProcessingTimeout mtrAccountLoginClusterLoginParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccountLoginClusterLoginParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tempAccountIdentifier@
tempAccountIdentifierSelector :: Selector
tempAccountIdentifierSelector = mkSelector "tempAccountIdentifier"

-- | @Selector@ for @setTempAccountIdentifier:@
setTempAccountIdentifierSelector :: Selector
setTempAccountIdentifierSelector = mkSelector "setTempAccountIdentifier:"

-- | @Selector@ for @setupPIN@
setupPINSelector :: Selector
setupPINSelector = mkSelector "setupPIN"

-- | @Selector@ for @setSetupPIN:@
setSetupPINSelector :: Selector
setSetupPINSelector = mkSelector "setSetupPIN:"

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @setNode:@
setNodeSelector :: Selector
setNodeSelector = mkSelector "setNode:"

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

