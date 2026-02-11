{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterMessageStruct@.
module ObjC.Matter.MTRMessagesClusterMessageStruct
  ( MTRMessagesClusterMessageStruct
  , IsMTRMessagesClusterMessageStruct(..)
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
messageID :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSData)
messageID mtrMessagesClusterMessageStruct  =
    sendMsg mtrMessagesClusterMessageStruct (mkSelector "messageID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageID:@
setMessageID :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSData value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setMessageID mtrMessagesClusterMessageStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageStruct (mkSelector "setMessageID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- priority@
priority :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSNumber)
priority mtrMessagesClusterMessageStruct  =
    sendMsg mtrMessagesClusterMessageStruct (mkSelector "priority") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPriority:@
setPriority :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSNumber value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setPriority mtrMessagesClusterMessageStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageStruct (mkSelector "setPriority:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- messageControl@
messageControl :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSNumber)
messageControl mtrMessagesClusterMessageStruct  =
    sendMsg mtrMessagesClusterMessageStruct (mkSelector "messageControl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageControl:@
setMessageControl :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSNumber value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setMessageControl mtrMessagesClusterMessageStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageStruct (mkSelector "setMessageControl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startTime@
startTime :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSNumber)
startTime mtrMessagesClusterMessageStruct  =
    sendMsg mtrMessagesClusterMessageStruct (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSNumber value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setStartTime mtrMessagesClusterMessageStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageStruct (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSNumber)
duration mtrMessagesClusterMessageStruct  =
    sendMsg mtrMessagesClusterMessageStruct (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSNumber value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setDuration mtrMessagesClusterMessageStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageStruct (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- messageText@
messageText :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSString)
messageText mtrMessagesClusterMessageStruct  =
    sendMsg mtrMessagesClusterMessageStruct (mkSelector "messageText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageText:@
setMessageText :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSString value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setMessageText mtrMessagesClusterMessageStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageStruct (mkSelector "setMessageText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- responses@
responses :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSArray)
responses mtrMessagesClusterMessageStruct  =
    sendMsg mtrMessagesClusterMessageStruct (mkSelector "responses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResponses:@
setResponses :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSArray value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setResponses mtrMessagesClusterMessageStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageStruct (mkSelector "setResponses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

