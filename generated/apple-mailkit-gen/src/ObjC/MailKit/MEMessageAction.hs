{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An action that can be performed on a mail message.
--
-- Generated bindings for @MEMessageAction@.
module ObjC.MailKit.MEMessageAction
  ( MEMessageAction
  , IsMEMessageAction(..)
  , flagActionWithFlag
  , setBackgroundColorActionWithColor
  , init_
  , new
  , moveToTrashAction
  , moveToArchiveAction
  , moveToJunkAction
  , markAsReadAction
  , markAsUnreadAction
  , flagActionWithFlagSelector
  , setBackgroundColorActionWithColorSelector
  , initSelector
  , newSelector
  , moveToTrashActionSelector
  , moveToArchiveActionSelector
  , moveToJunkActionSelector
  , markAsReadActionSelector
  , markAsUnreadActionSelector

  -- * Enum types
  , MEMessageActionFlag(MEMessageActionFlag)
  , pattern MEMessageActionFlagNone
  , pattern MEMessageActionFlagDefaultColor
  , pattern MEMessageActionFlagRed
  , pattern MEMessageActionFlagOrange
  , pattern MEMessageActionFlagYellow
  , pattern MEMessageActionFlagGreen
  , pattern MEMessageActionFlagBlue
  , pattern MEMessageActionFlagPurple
  , pattern MEMessageActionFlagGray
  , MEMessageActionMessageColor(MEMessageActionMessageColor)
  , pattern MEMessageActionMessageColorNone
  , pattern MEMessageActionMessageColorGreen
  , pattern MEMessageActionMessageColorYellow
  , pattern MEMessageActionMessageColorOrange
  , pattern MEMessageActionMessageColorRed
  , pattern MEMessageActionMessageColorPurple
  , pattern MEMessageActionMessageColorBlue
  , pattern MEMessageActionMessageColorGray

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

-- | Marks the message as flagged with the provided color.
--
-- ObjC selector: @+ flagActionWithFlag:@
flagActionWithFlag :: MEMessageActionFlag -> IO (Id MEMessageAction)
flagActionWithFlag flag =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMsg cls' (mkSelector "flagActionWithFlag:") (retPtr retVoid) [argCLong (coerce flag)] >>= retainedObject . castPtr

-- | Adds a color to the message when shown in the message list.
--
-- ObjC selector: @+ setBackgroundColorActionWithColor:@
setBackgroundColorActionWithColor :: MEMessageActionMessageColor -> IO (Id MEMessageAction)
setBackgroundColorActionWithColor color =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMsg cls' (mkSelector "setBackgroundColorActionWithColor:") (retPtr retVoid) [argCLong (coerce color)] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMEMessageAction meMessageAction => meMessageAction -> IO (Id MEMessageAction)
init_ meMessageAction  =
    sendMsg meMessageAction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MEMessageAction)
new  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Moves the mail message to the user's trash mailbox for the account.
--
-- ObjC selector: @+ moveToTrashAction@
moveToTrashAction :: IO (Id MEMessageAction)
moveToTrashAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMsg cls' (mkSelector "moveToTrashAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Moves the mail message to the user's archive mailbox for the account.
--
-- ObjC selector: @+ moveToArchiveAction@
moveToArchiveAction :: IO (Id MEMessageAction)
moveToArchiveAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMsg cls' (mkSelector "moveToArchiveAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Moves the mail message to the user's junk mailbox for the account.
--
-- ObjC selector: @+ moveToJunkAction@
moveToJunkAction :: IO (Id MEMessageAction)
moveToJunkAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMsg cls' (mkSelector "moveToJunkAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Marks the mail message as read.
--
-- ObjC selector: @+ markAsReadAction@
markAsReadAction :: IO (Id MEMessageAction)
markAsReadAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMsg cls' (mkSelector "markAsReadAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Marks the mail  message as unread.
--
-- ObjC selector: @+ markAsUnreadAction@
markAsUnreadAction :: IO (Id MEMessageAction)
markAsUnreadAction  =
  do
    cls' <- getRequiredClass "MEMessageAction"
    sendClassMsg cls' (mkSelector "markAsUnreadAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @flagActionWithFlag:@
flagActionWithFlagSelector :: Selector
flagActionWithFlagSelector = mkSelector "flagActionWithFlag:"

-- | @Selector@ for @setBackgroundColorActionWithColor:@
setBackgroundColorActionWithColorSelector :: Selector
setBackgroundColorActionWithColorSelector = mkSelector "setBackgroundColorActionWithColor:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @moveToTrashAction@
moveToTrashActionSelector :: Selector
moveToTrashActionSelector = mkSelector "moveToTrashAction"

-- | @Selector@ for @moveToArchiveAction@
moveToArchiveActionSelector :: Selector
moveToArchiveActionSelector = mkSelector "moveToArchiveAction"

-- | @Selector@ for @moveToJunkAction@
moveToJunkActionSelector :: Selector
moveToJunkActionSelector = mkSelector "moveToJunkAction"

-- | @Selector@ for @markAsReadAction@
markAsReadActionSelector :: Selector
markAsReadActionSelector = mkSelector "markAsReadAction"

-- | @Selector@ for @markAsUnreadAction@
markAsUnreadActionSelector :: Selector
markAsUnreadActionSelector = mkSelector "markAsUnreadAction"

