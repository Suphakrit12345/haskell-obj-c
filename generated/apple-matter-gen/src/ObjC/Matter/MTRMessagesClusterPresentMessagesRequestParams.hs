{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterPresentMessagesRequestParams@.
module ObjC.Matter.MTRMessagesClusterPresentMessagesRequestParams
  ( MTRMessagesClusterPresentMessagesRequestParams
  , IsMTRMessagesClusterPresentMessagesRequestParams(..)
  , messageID
  , setMessageID
  , priority
  , setPriority
  , messageControl
  , setMessageControl
  , startTime
  , setStartTime
  , duration
  , setDuration
  , messageText
  , setMessageText
  , responses
  , setResponses
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , messageIDSelector
  , setMessageIDSelector
  , prioritySelector
  , setPrioritySelector
  , messageControlSelector
  , setMessageControlSelector
  , startTimeSelector
  , setStartTimeSelector
  , durationSelector
  , setDurationSelector
  , messageTextSelector
  , setMessageTextSelector
  , responsesSelector
  , setResponsesSelector
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

-- | @- messageID@
messageID :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSData)
messageID mtrMessagesClusterPresentMessagesRequestParams  =
    sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "messageID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageID:@
setMessageID :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSData value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setMessageID mtrMessagesClusterPresentMessagesRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "setMessageID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- priority@
priority :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
priority mtrMessagesClusterPresentMessagesRequestParams  =
    sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "priority") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPriority:@
setPriority :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setPriority mtrMessagesClusterPresentMessagesRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "setPriority:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- messageControl@
messageControl :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
messageControl mtrMessagesClusterPresentMessagesRequestParams  =
    sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "messageControl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageControl:@
setMessageControl :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setMessageControl mtrMessagesClusterPresentMessagesRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "setMessageControl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startTime@
startTime :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
startTime mtrMessagesClusterPresentMessagesRequestParams  =
    sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setStartTime mtrMessagesClusterPresentMessagesRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
duration mtrMessagesClusterPresentMessagesRequestParams  =
    sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setDuration mtrMessagesClusterPresentMessagesRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- messageText@
messageText :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSString)
messageText mtrMessagesClusterPresentMessagesRequestParams  =
    sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "messageText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageText:@
setMessageText :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSString value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setMessageText mtrMessagesClusterPresentMessagesRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "setMessageText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- responses@
responses :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSArray)
responses mtrMessagesClusterPresentMessagesRequestParams  =
    sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "responses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResponses:@
setResponses :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSArray value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setResponses mtrMessagesClusterPresentMessagesRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "setResponses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMessagesClusterPresentMessagesRequestParams  =
    sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMessagesClusterPresentMessagesRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMessagesClusterPresentMessagesRequestParams  =
    sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrMessagesClusterPresentMessagesRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterPresentMessagesRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageID@
messageIDSelector :: Selector
messageIDSelector = mkSelector "messageID"

-- | @Selector@ for @setMessageID:@
setMessageIDSelector :: Selector
setMessageIDSelector = mkSelector "setMessageID:"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @messageControl@
messageControlSelector :: Selector
messageControlSelector = mkSelector "messageControl"

-- | @Selector@ for @setMessageControl:@
setMessageControlSelector :: Selector
setMessageControlSelector = mkSelector "setMessageControl:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @messageText@
messageTextSelector :: Selector
messageTextSelector = mkSelector "messageText"

-- | @Selector@ for @setMessageText:@
setMessageTextSelector :: Selector
setMessageTextSelector = mkSelector "setMessageText:"

-- | @Selector@ for @responses@
responsesSelector :: Selector
responsesSelector = mkSelector "responses"

-- | @Selector@ for @setResponses:@
setResponsesSelector :: Selector
setResponsesSelector = mkSelector "setResponses:"

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

