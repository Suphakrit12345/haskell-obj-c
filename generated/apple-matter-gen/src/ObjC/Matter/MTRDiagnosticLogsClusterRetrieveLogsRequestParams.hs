{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDiagnosticLogsClusterRetrieveLogsRequestParams@.
module ObjC.Matter.MTRDiagnosticLogsClusterRetrieveLogsRequestParams
  ( MTRDiagnosticLogsClusterRetrieveLogsRequestParams
  , IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams(..)
  , intent
  , setIntent
  , requestedProtocol
  , setRequestedProtocol
  , transferFileDesignator
  , setTransferFileDesignator
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , intentSelector
  , setIntentSelector
  , requestedProtocolSelector
  , setRequestedProtocolSelector
  , transferFileDesignatorSelector
  , setTransferFileDesignatorSelector
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

-- | @- intent@
intent :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSNumber)
intent mtrDiagnosticLogsClusterRetrieveLogsRequestParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "intent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIntent:@
setIntent :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setIntent mtrDiagnosticLogsClusterRetrieveLogsRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "setIntent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requestedProtocol@
requestedProtocol :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSNumber)
requestedProtocol mtrDiagnosticLogsClusterRetrieveLogsRequestParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "requestedProtocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequestedProtocol:@
setRequestedProtocol :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setRequestedProtocol mtrDiagnosticLogsClusterRetrieveLogsRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "setRequestedProtocol:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transferFileDesignator@
transferFileDesignator :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSString)
transferFileDesignator mtrDiagnosticLogsClusterRetrieveLogsRequestParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "transferFileDesignator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransferFileDesignator:@
setTransferFileDesignator :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSString value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setTransferFileDesignator mtrDiagnosticLogsClusterRetrieveLogsRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "setTransferFileDesignator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDiagnosticLogsClusterRetrieveLogsRequestParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDiagnosticLogsClusterRetrieveLogsRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDiagnosticLogsClusterRetrieveLogsRequestParams  =
    sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrDiagnosticLogsClusterRetrieveLogsRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDiagnosticLogsClusterRetrieveLogsRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @intent@
intentSelector :: Selector
intentSelector = mkSelector "intent"

-- | @Selector@ for @setIntent:@
setIntentSelector :: Selector
setIntentSelector = mkSelector "setIntent:"

-- | @Selector@ for @requestedProtocol@
requestedProtocolSelector :: Selector
requestedProtocolSelector = mkSelector "requestedProtocol"

-- | @Selector@ for @setRequestedProtocol:@
setRequestedProtocolSelector :: Selector
setRequestedProtocolSelector = mkSelector "setRequestedProtocol:"

-- | @Selector@ for @transferFileDesignator@
transferFileDesignatorSelector :: Selector
transferFileDesignatorSelector = mkSelector "transferFileDesignator"

-- | @Selector@ for @setTransferFileDesignator:@
setTransferFileDesignatorSelector :: Selector
setTransferFileDesignatorSelector = mkSelector "setTransferFileDesignator:"

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

