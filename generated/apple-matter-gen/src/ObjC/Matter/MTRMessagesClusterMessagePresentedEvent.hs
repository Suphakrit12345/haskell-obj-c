{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterMessagePresentedEvent@.
module ObjC.Matter.MTRMessagesClusterMessagePresentedEvent
  ( MTRMessagesClusterMessagePresentedEvent
  , IsMTRMessagesClusterMessagePresentedEvent(..)
  , messageID
  , setMessageID
  , messageIDSelector
  , setMessageIDSelector


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
messageID :: IsMTRMessagesClusterMessagePresentedEvent mtrMessagesClusterMessagePresentedEvent => mtrMessagesClusterMessagePresentedEvent -> IO (Id NSData)
messageID mtrMessagesClusterMessagePresentedEvent  =
    sendMsg mtrMessagesClusterMessagePresentedEvent (mkSelector "messageID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageID:@
setMessageID :: (IsMTRMessagesClusterMessagePresentedEvent mtrMessagesClusterMessagePresentedEvent, IsNSData value) => mtrMessagesClusterMessagePresentedEvent -> value -> IO ()
setMessageID mtrMessagesClusterMessagePresentedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessagePresentedEvent (mkSelector "setMessageID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageID@
messageIDSelector :: Selector
messageIDSelector = mkSelector "messageID"

-- | @Selector@ for @setMessageID:@
setMessageIDSelector :: Selector
setMessageIDSelector = mkSelector "setMessageID:"

