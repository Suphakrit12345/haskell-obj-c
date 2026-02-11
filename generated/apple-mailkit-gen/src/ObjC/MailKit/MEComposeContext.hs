{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object encapsulating additional information about the message being composed.
--
-- Generated bindings for @MEComposeContext@.
module ObjC.MailKit.MEComposeContext
  ( MEComposeContext
  , IsMEComposeContext(..)
  , contextID
  , originalMessage
  , action
  , isEncrypted
  , shouldEncrypt
  , isSigned
  , shouldSign
  , contextIDSelector
  , originalMessageSelector
  , actionSelector
  , isEncryptedSelector
  , shouldEncryptSelector
  , isSignedSelector
  , shouldSignSelector

  -- * Enum types
  , MEComposeUserAction(MEComposeUserAction)
  , pattern MEComposeUserActionNewMessage
  , pattern MEComposeUserActionReply
  , pattern MEComposeUserActionReplyAll
  , pattern MEComposeUserActionForward

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

import ObjC.MailKit.Internal.Classes
import ObjC.MailKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | A unique identifier for the compose context.
--
-- ObjC selector: @- contextID@
contextID :: IsMEComposeContext meComposeContext => meComposeContext -> IO (Id NSUUID)
contextID meComposeContext  =
    sendMsg meComposeContext (mkSelector "contextID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The original email message on which user performed an action It is @nil@ for @MEComposeUserActionNewMessage@ actions.
--
-- ObjC selector: @- originalMessage@
originalMessage :: IsMEComposeContext meComposeContext => meComposeContext -> IO (Id MEMessage)
originalMessage meComposeContext  =
    sendMsg meComposeContext (mkSelector "originalMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the action performed by the user that created this compose context.
--
-- ObjC selector: @- action@
action :: IsMEComposeContext meComposeContext => meComposeContext -> IO MEComposeUserAction
action meComposeContext  =
    fmap (coerce :: CLong -> MEComposeUserAction) $ sendMsg meComposeContext (mkSelector "action") retCLong []

-- | Boolean that indicates the message is encrypted by a Message Security extension.
--
-- ObjC selector: @- isEncrypted@
isEncrypted :: IsMEComposeContext meComposeContext => meComposeContext -> IO Bool
isEncrypted meComposeContext  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meComposeContext (mkSelector "isEncrypted") retCULong []

-- | Boolean that indicates if the user wants to encrypt the message.
--
-- ObjC selector: @- shouldEncrypt@
shouldEncrypt :: IsMEComposeContext meComposeContext => meComposeContext -> IO Bool
shouldEncrypt meComposeContext  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meComposeContext (mkSelector "shouldEncrypt") retCULong []

-- | Boolean that indicates the message is signed by a Message Security extension.
--
-- ObjC selector: @- isSigned@
isSigned :: IsMEComposeContext meComposeContext => meComposeContext -> IO Bool
isSigned meComposeContext  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meComposeContext (mkSelector "isSigned") retCULong []

-- | A Boolean that indicates if the user wants to sign the message.
--
-- ObjC selector: @- shouldSign@
shouldSign :: IsMEComposeContext meComposeContext => meComposeContext -> IO Bool
shouldSign meComposeContext  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meComposeContext (mkSelector "shouldSign") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contextID@
contextIDSelector :: Selector
contextIDSelector = mkSelector "contextID"

-- | @Selector@ for @originalMessage@
originalMessageSelector :: Selector
originalMessageSelector = mkSelector "originalMessage"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @isEncrypted@
isEncryptedSelector :: Selector
isEncryptedSelector = mkSelector "isEncrypted"

-- | @Selector@ for @shouldEncrypt@
shouldEncryptSelector :: Selector
shouldEncryptSelector = mkSelector "shouldEncrypt"

-- | @Selector@ for @isSigned@
isSignedSelector :: Selector
isSignedSelector = mkSelector "isSigned"

-- | @Selector@ for @shouldSign@
shouldSignSelector :: Selector
shouldSignSelector = mkSelector "shouldSign"

