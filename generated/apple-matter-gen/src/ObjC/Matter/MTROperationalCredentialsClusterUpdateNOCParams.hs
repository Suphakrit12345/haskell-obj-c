{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterUpdateNOCParams@.
module ObjC.Matter.MTROperationalCredentialsClusterUpdateNOCParams
  ( MTROperationalCredentialsClusterUpdateNOCParams
  , IsMTROperationalCredentialsClusterUpdateNOCParams(..)
  , nocValue
  , setNocValue
  , icacValue
  , setIcacValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , nocValueSelector
  , setNocValueSelector
  , icacValueSelector
  , setIcacValueSelector
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

-- | @- nocValue@
nocValue :: IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams => mtrOperationalCredentialsClusterUpdateNOCParams -> IO (Id NSData)
nocValue mtrOperationalCredentialsClusterUpdateNOCParams  =
    sendMsg mtrOperationalCredentialsClusterUpdateNOCParams (mkSelector "nocValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNocValue:@
setNocValue :: (IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams, IsNSData value) => mtrOperationalCredentialsClusterUpdateNOCParams -> value -> IO ()
setNocValue mtrOperationalCredentialsClusterUpdateNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterUpdateNOCParams (mkSelector "setNocValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- icacValue@
icacValue :: IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams => mtrOperationalCredentialsClusterUpdateNOCParams -> IO (Id NSData)
icacValue mtrOperationalCredentialsClusterUpdateNOCParams  =
    sendMsg mtrOperationalCredentialsClusterUpdateNOCParams (mkSelector "icacValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIcacValue:@
setIcacValue :: (IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams, IsNSData value) => mtrOperationalCredentialsClusterUpdateNOCParams -> value -> IO ()
setIcacValue mtrOperationalCredentialsClusterUpdateNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterUpdateNOCParams (mkSelector "setIcacValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams => mtrOperationalCredentialsClusterUpdateNOCParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterUpdateNOCParams  =
    sendMsg mtrOperationalCredentialsClusterUpdateNOCParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterUpdateNOCParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterUpdateNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterUpdateNOCParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams => mtrOperationalCredentialsClusterUpdateNOCParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterUpdateNOCParams  =
    sendMsg mtrOperationalCredentialsClusterUpdateNOCParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterUpdateNOCParams mtrOperationalCredentialsClusterUpdateNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterUpdateNOCParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterUpdateNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterUpdateNOCParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nocValue@
nocValueSelector :: Selector
nocValueSelector = mkSelector "nocValue"

-- | @Selector@ for @setNocValue:@
setNocValueSelector :: Selector
setNocValueSelector = mkSelector "setNocValue:"

-- | @Selector@ for @icacValue@
icacValueSelector :: Selector
icacValueSelector = mkSelector "icacValue"

-- | @Selector@ for @setIcacValue:@
setIcacValueSelector :: Selector
setIcacValueSelector = mkSelector "setIcacValue:"

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

