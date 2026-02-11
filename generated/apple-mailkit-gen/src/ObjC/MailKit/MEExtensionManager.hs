{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Methods in this class allow the host app to interact with their Mail extension.
--
-- Generated bindings for @MEExtensionManager@.
module ObjC.MailKit.MEExtensionManager
  ( MEExtensionManager
  , IsMEExtensionManager(..)
  , new
  , init_
  , reloadContentBlockerWithIdentifier_completionHandler
  , reloadVisibleMessagesWithCompletionHandler
  , newSelector
  , initSelector
  , reloadContentBlockerWithIdentifier_completionHandlerSelector
  , reloadVisibleMessagesWithCompletionHandlerSelector


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
new :: IO (Id MEExtensionManager)
new  =
  do
    cls' <- getRequiredClass "MEExtensionManager"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEExtensionManager meExtensionManager => meExtensionManager -> IO (Id MEExtensionManager)
init_ meExtensionManager  =
    sendMsg meExtensionManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | This will call on Mail to reload the content rule list associated with the given identifier. Mail May throttle reloading the content blocker to once every few minutes.
--
-- ObjC selector: @+ reloadContentBlockerWithIdentifier:completionHandler:@
reloadContentBlockerWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
reloadContentBlockerWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "MEExtensionManager"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "reloadContentBlockerWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | This will call on Mail to reload the currently visible messages.  Mail may throttle reloading visible messages.
--
-- ObjC selector: @+ reloadVisibleMessagesWithCompletionHandler:@
reloadVisibleMessagesWithCompletionHandler :: Ptr () -> IO ()
reloadVisibleMessagesWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "MEExtensionManager"
    sendClassMsg cls' (mkSelector "reloadVisibleMessagesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @reloadContentBlockerWithIdentifier:completionHandler:@
reloadContentBlockerWithIdentifier_completionHandlerSelector :: Selector
reloadContentBlockerWithIdentifier_completionHandlerSelector = mkSelector "reloadContentBlockerWithIdentifier:completionHandler:"

-- | @Selector@ for @reloadVisibleMessagesWithCompletionHandler:@
reloadVisibleMessagesWithCompletionHandlerSelector :: Selector
reloadVisibleMessagesWithCompletionHandlerSelector = mkSelector "reloadVisibleMessagesWithCompletionHandler:"

