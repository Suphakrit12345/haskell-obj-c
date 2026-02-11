{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterPayloadTestRequestParams@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterPayloadTestRequestParams
  ( MTRGeneralDiagnosticsClusterPayloadTestRequestParams
  , IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams(..)
  , enableKey
  , setEnableKey
  , value
  , setValue
  , count
  , setCount
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , enableKeySelector
  , setEnableKeySelector
  , valueSelector
  , setValueSelector
  , countSelector
  , setCountSelector
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

-- | @- enableKey@
enableKey :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSData)
enableKey mtrGeneralDiagnosticsClusterPayloadTestRequestParams  =
    sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "enableKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnableKey:@
setEnableKey :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSData value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setEnableKey mtrGeneralDiagnosticsClusterPayloadTestRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "setEnableKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSNumber)
value mtrGeneralDiagnosticsClusterPayloadTestRequestParams  =
    sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setValue mtrGeneralDiagnosticsClusterPayloadTestRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- count@
count :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSNumber)
count mtrGeneralDiagnosticsClusterPayloadTestRequestParams  =
    sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "count") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCount:@
setCount :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setCount mtrGeneralDiagnosticsClusterPayloadTestRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "setCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGeneralDiagnosticsClusterPayloadTestRequestParams  =
    sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGeneralDiagnosticsClusterPayloadTestRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGeneralDiagnosticsClusterPayloadTestRequestParams  =
    sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrGeneralDiagnosticsClusterPayloadTestRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterPayloadTestRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enableKey@
enableKeySelector :: Selector
enableKeySelector = mkSelector "enableKey"

-- | @Selector@ for @setEnableKey:@
setEnableKeySelector :: Selector
setEnableKeySelector = mkSelector "setEnableKey:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @setCount:@
setCountSelector :: Selector
setCountSelector = mkSelector "setCount:"

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

