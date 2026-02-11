{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An instance of this class is associated with the lifecycle of a single mail compose window. This object associates the actions performed by the user in a mail compose window to a unique session. An instance of this class is passed to the methods in @MEComposeSessionHandler.@
--
-- Generated bindings for @MEComposeSession@.
module ObjC.MailKit.MEComposeSession
  ( MEComposeSession
  , IsMEComposeSession(..)
  , new
  , init_
  , reloadSession
  , sessionID
  , mailMessage
  , composeContext
  , newSelector
  , initSelector
  , reloadSessionSelector
  , sessionIDSelector
  , mailMessageSelector
  , composeContextSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MEComposeSession)
new  =
  do
    cls' <- getRequiredClass "MEComposeSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEComposeSession meComposeSession => meComposeSession -> IO (Id MEComposeSession)
init_ meComposeSession  =
    sendMsg meComposeSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Requests Mail to refresh compose session with new information that the extension has.
--
-- Extensions can use this call this method to regenerate @MEAddressAnnotation@ instances to replace those that were previously generated for this session. This will result in invocations to @-[MEComposeSessionHandler@ @session:annotateAddressesWithCompletionHandler:].@
--
-- ObjC selector: @- reloadSession@
reloadSession :: IsMEComposeSession meComposeSession => meComposeSession -> IO ()
reloadSession meComposeSession  =
    sendMsg meComposeSession (mkSelector "reloadSession") retVoid []

-- | A unique identifier for the session.
--
-- ObjC selector: @- sessionID@
sessionID :: IsMEComposeSession meComposeSession => meComposeSession -> IO (Id NSUUID)
sessionID meComposeSession  =
    sendMsg meComposeSession (mkSelector "sessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An instance of @MEMessage@ that represents properties of the mail message that author is composing in this @MEComposeSession@
--
-- ObjC selector: @- mailMessage@
mailMessage :: IsMEComposeSession meComposeSession => meComposeSession -> IO (Id MEMessage)
mailMessage meComposeSession  =
    sendMsg meComposeSession (mkSelector "mailMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An instance of @MEComposeContext@ that provides additional information about the compose session.
--
-- ObjC selector: @- composeContext@
composeContext :: IsMEComposeSession meComposeSession => meComposeSession -> IO (Id MEComposeContext)
composeContext meComposeSession  =
    sendMsg meComposeSession (mkSelector "composeContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @reloadSession@
reloadSessionSelector :: Selector
reloadSessionSelector = mkSelector "reloadSession"

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @mailMessage@
mailMessageSelector :: Selector
mailMessageSelector = mkSelector "mailMessage"

-- | @Selector@ for @composeContext@
composeContextSelector :: Selector
composeContextSelector = mkSelector "composeContext"

