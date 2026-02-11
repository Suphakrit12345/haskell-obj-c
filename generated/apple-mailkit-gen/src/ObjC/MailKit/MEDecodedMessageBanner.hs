{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains security information in order to populate a banner in the message view.
--
-- Generated bindings for @MEDecodedMessageBanner@.
module ObjC.MailKit.MEDecodedMessageBanner
  ( MEDecodedMessageBanner
  , IsMEDecodedMessageBanner(..)
  , new
  , init_
  , initWithTitle_primaryActionTitle_dismissable
  , title
  , primaryActionTitle
  , dismissable
  , newSelector
  , initSelector
  , initWithTitle_primaryActionTitle_dismissableSelector
  , titleSelector
  , primaryActionTitleSelector
  , dismissableSelector


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
new :: IO (Id MEDecodedMessageBanner)
new  =
  do
    cls' <- getRequiredClass "MEDecodedMessageBanner"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEDecodedMessageBanner meDecodedMessageBanner => meDecodedMessageBanner -> IO (Id MEDecodedMessageBanner)
init_ meDecodedMessageBanner  =
    sendMsg meDecodedMessageBanner (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithTitle:primaryActionTitle:dismissable:@
initWithTitle_primaryActionTitle_dismissable :: (IsMEDecodedMessageBanner meDecodedMessageBanner, IsNSString title, IsNSString primaryActionTitle) => meDecodedMessageBanner -> title -> primaryActionTitle -> Bool -> IO (Id MEDecodedMessageBanner)
initWithTitle_primaryActionTitle_dismissable meDecodedMessageBanner  title primaryActionTitle dismissable =
  withObjCPtr title $ \raw_title ->
    withObjCPtr primaryActionTitle $ \raw_primaryActionTitle ->
        sendMsg meDecodedMessageBanner (mkSelector "initWithTitle:primaryActionTitle:dismissable:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_primaryActionTitle :: Ptr ()), argCULong (if dismissable then 1 else 0)] >>= ownedObject . castPtr

-- | @- title@
title :: IsMEDecodedMessageBanner meDecodedMessageBanner => meDecodedMessageBanner -> IO (Id NSString)
title meDecodedMessageBanner  =
    sendMsg meDecodedMessageBanner (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- primaryActionTitle@
primaryActionTitle :: IsMEDecodedMessageBanner meDecodedMessageBanner => meDecodedMessageBanner -> IO (Id NSString)
primaryActionTitle meDecodedMessageBanner  =
    sendMsg meDecodedMessageBanner (mkSelector "primaryActionTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dismissable@
dismissable :: IsMEDecodedMessageBanner meDecodedMessageBanner => meDecodedMessageBanner -> IO Bool
dismissable meDecodedMessageBanner  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meDecodedMessageBanner (mkSelector "dismissable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTitle:primaryActionTitle:dismissable:@
initWithTitle_primaryActionTitle_dismissableSelector :: Selector
initWithTitle_primaryActionTitle_dismissableSelector = mkSelector "initWithTitle:primaryActionTitle:dismissable:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @primaryActionTitle@
primaryActionTitleSelector :: Selector
primaryActionTitleSelector = mkSelector "primaryActionTitle"

-- | @Selector@ for @dismissable@
dismissableSelector :: Selector
dismissableSelector = mkSelector "dismissable"

