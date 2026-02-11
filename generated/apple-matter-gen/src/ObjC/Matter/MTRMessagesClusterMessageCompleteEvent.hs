{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterMessageCompleteEvent@.
module ObjC.Matter.MTRMessagesClusterMessageCompleteEvent
  ( MTRMessagesClusterMessageCompleteEvent
  , IsMTRMessagesClusterMessageCompleteEvent(..)
  , messageID
  , setMessageID
  , responseID
  , setResponseID
  , reply
  , setReply
  , futureMessagesPreference
  , setFutureMessagesPreference
  , messageIDSelector
  , setMessageIDSelector
  , responseIDSelector
  , setResponseIDSelector
  , replySelector
  , setReplySelector
  , futureMessagesPreferenceSelector
  , setFutureMessagesPreferenceSelector


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
messageID :: IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent => mtrMessagesClusterMessageCompleteEvent -> IO (Id NSData)
messageID mtrMessagesClusterMessageCompleteEvent  =
    sendMsg mtrMessagesClusterMessageCompleteEvent (mkSelector "messageID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageID:@
setMessageID :: (IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent, IsNSData value) => mtrMessagesClusterMessageCompleteEvent -> value -> IO ()
setMessageID mtrMessagesClusterMessageCompleteEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageCompleteEvent (mkSelector "setMessageID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- responseID@
responseID :: IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent => mtrMessagesClusterMessageCompleteEvent -> IO (Id NSNumber)
responseID mtrMessagesClusterMessageCompleteEvent  =
    sendMsg mtrMessagesClusterMessageCompleteEvent (mkSelector "responseID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResponseID:@
setResponseID :: (IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent, IsNSNumber value) => mtrMessagesClusterMessageCompleteEvent -> value -> IO ()
setResponseID mtrMessagesClusterMessageCompleteEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageCompleteEvent (mkSelector "setResponseID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reply@
reply :: IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent => mtrMessagesClusterMessageCompleteEvent -> IO (Id NSString)
reply mtrMessagesClusterMessageCompleteEvent  =
    sendMsg mtrMessagesClusterMessageCompleteEvent (mkSelector "reply") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReply:@
setReply :: (IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent, IsNSString value) => mtrMessagesClusterMessageCompleteEvent -> value -> IO ()
setReply mtrMessagesClusterMessageCompleteEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageCompleteEvent (mkSelector "setReply:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- futureMessagesPreference@
futureMessagesPreference :: IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent => mtrMessagesClusterMessageCompleteEvent -> IO (Id NSNumber)
futureMessagesPreference mtrMessagesClusterMessageCompleteEvent  =
    sendMsg mtrMessagesClusterMessageCompleteEvent (mkSelector "futureMessagesPreference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFutureMessagesPreference:@
setFutureMessagesPreference :: (IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent, IsNSNumber value) => mtrMessagesClusterMessageCompleteEvent -> value -> IO ()
setFutureMessagesPreference mtrMessagesClusterMessageCompleteEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageCompleteEvent (mkSelector "setFutureMessagesPreference:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageID@
messageIDSelector :: Selector
messageIDSelector = mkSelector "messageID"

-- | @Selector@ for @setMessageID:@
setMessageIDSelector :: Selector
setMessageIDSelector = mkSelector "setMessageID:"

-- | @Selector@ for @responseID@
responseIDSelector :: Selector
responseIDSelector = mkSelector "responseID"

-- | @Selector@ for @setResponseID:@
setResponseIDSelector :: Selector
setResponseIDSelector = mkSelector "setResponseID:"

-- | @Selector@ for @reply@
replySelector :: Selector
replySelector = mkSelector "reply"

-- | @Selector@ for @setReply:@
setReplySelector :: Selector
setReplySelector = mkSelector "setReply:"

-- | @Selector@ for @futureMessagesPreference@
futureMessagesPreferenceSelector :: Selector
futureMessagesPreferenceSelector = mkSelector "futureMessagesPreference"

-- | @Selector@ for @setFutureMessagesPreference:@
setFutureMessagesPreferenceSelector :: Selector
setFutureMessagesPreferenceSelector = mkSelector "setFutureMessagesPreference:"

