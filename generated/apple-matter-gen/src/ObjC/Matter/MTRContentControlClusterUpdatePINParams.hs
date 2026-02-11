{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentControlClusterUpdatePINParams@.
module ObjC.Matter.MTRContentControlClusterUpdatePINParams
  ( MTRContentControlClusterUpdatePINParams
  , IsMTRContentControlClusterUpdatePINParams(..)
  , oldPIN
  , setOldPIN
  , newPIN
  , setNewPIN
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , oldPINSelector
  , setOldPINSelector
  , newPINSelector
  , setNewPINSelector
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

-- | @- oldPIN@
oldPIN :: IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams => mtrContentControlClusterUpdatePINParams -> IO (Id NSString)
oldPIN mtrContentControlClusterUpdatePINParams  =
    sendMsg mtrContentControlClusterUpdatePINParams (mkSelector "oldPIN") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOldPIN:@
setOldPIN :: (IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams, IsNSString value) => mtrContentControlClusterUpdatePINParams -> value -> IO ()
setOldPIN mtrContentControlClusterUpdatePINParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentControlClusterUpdatePINParams (mkSelector "setOldPIN:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- newPIN@
newPIN :: IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams => mtrContentControlClusterUpdatePINParams -> IO (Id NSString)
newPIN mtrContentControlClusterUpdatePINParams  =
    sendMsg mtrContentControlClusterUpdatePINParams (mkSelector "newPIN") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewPIN:@
setNewPIN :: (IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams, IsNSString value) => mtrContentControlClusterUpdatePINParams -> value -> IO ()
setNewPIN mtrContentControlClusterUpdatePINParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentControlClusterUpdatePINParams (mkSelector "setNewPIN:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams => mtrContentControlClusterUpdatePINParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentControlClusterUpdatePINParams  =
    sendMsg mtrContentControlClusterUpdatePINParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams, IsNSNumber value) => mtrContentControlClusterUpdatePINParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentControlClusterUpdatePINParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentControlClusterUpdatePINParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams => mtrContentControlClusterUpdatePINParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentControlClusterUpdatePINParams  =
    sendMsg mtrContentControlClusterUpdatePINParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams, IsNSNumber value) => mtrContentControlClusterUpdatePINParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentControlClusterUpdatePINParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentControlClusterUpdatePINParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @oldPIN@
oldPINSelector :: Selector
oldPINSelector = mkSelector "oldPIN"

-- | @Selector@ for @setOldPIN:@
setOldPINSelector :: Selector
setOldPINSelector = mkSelector "setOldPIN:"

-- | @Selector@ for @newPIN@
newPINSelector :: Selector
newPINSelector = mkSelector "newPIN"

-- | @Selector@ for @setNewPIN:@
setNewPINSelector :: Selector
setNewPINSelector = mkSelector "setNewPIN:"

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

