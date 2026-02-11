{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams@.
module ObjC.Matter.MTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams
  ( MTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams
  , IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams(..)
  , announcementReason
  , setAnnouncementReason
  , metadataForNode
  , setMetadataForNode
  , endpoint
  , setEndpoint
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , announcementReasonSelector
  , setAnnouncementReasonSelector
  , metadataForNodeSelector
  , setMetadataForNodeSelector
  , endpointSelector
  , setEndpointSelector
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

-- | @- announcementReason@
announcementReason :: IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> IO (Id NSNumber)
announcementReason mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "announcementReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAnnouncementReason:@
setAnnouncementReason :: (IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> value -> IO ()
setAnnouncementReason mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "setAnnouncementReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metadataForNode@
metadataForNode :: IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> IO (Id NSData)
metadataForNode mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "metadataForNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetadataForNode:@
setMetadataForNode :: (IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams, IsNSData value) => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> value -> IO ()
setMetadataForNode mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "setMetadataForNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> IO (Id NSNumber)
endpoint mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> value -> IO ()
setEndpoint mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams -> value -> IO ()
setServerSideProcessingTimeout mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterAnnounceOtaProviderParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @announcementReason@
announcementReasonSelector :: Selector
announcementReasonSelector = mkSelector "announcementReason"

-- | @Selector@ for @setAnnouncementReason:@
setAnnouncementReasonSelector :: Selector
setAnnouncementReasonSelector = mkSelector "setAnnouncementReason:"

-- | @Selector@ for @metadataForNode@
metadataForNodeSelector :: Selector
metadataForNodeSelector = mkSelector "metadataForNode"

-- | @Selector@ for @setMetadataForNode:@
setMetadataForNodeSelector :: Selector
setMetadataForNodeSelector = mkSelector "setMetadataForNode:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

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

