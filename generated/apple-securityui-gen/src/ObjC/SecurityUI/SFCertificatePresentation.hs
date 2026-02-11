{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFCertificatePresentation@.
module ObjC.SecurityUI.SFCertificatePresentation
  ( SFCertificatePresentation
  , IsSFCertificatePresentation(..)
  , initWithTrust
  , init_
  , new
  , presentSheetInWindow_dismissHandler
  , dismissSheet
  , trust
  , title
  , setTitle
  , message
  , setMessage
  , helpURL
  , setHelpURL
  , initWithTrustSelector
  , initSelector
  , newSelector
  , presentSheetInWindow_dismissHandlerSelector
  , dismissSheetSelector
  , trustSelector
  , titleSelector
  , setTitleSelector
  , messageSelector
  , setMessageSelector
  , helpURLSelector
  , setHelpURLSelector


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

import ObjC.SecurityUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the certificate presentation with a certificate trust reference.
--
-- ObjC selector: @- initWithTrust:@
initWithTrust :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> Ptr () -> IO (Id SFCertificatePresentation)
initWithTrust sfCertificatePresentation  trust =
    sendMsg sfCertificatePresentation (mkSelector "initWithTrust:") (retPtr retVoid) [argPtr trust] >>= ownedObject . castPtr

-- | Clients should use designated initializers.
--
-- ObjC selector: @- init@
init_ :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Id SFCertificatePresentation)
init_ sfCertificatePresentation  =
    sendMsg sfCertificatePresentation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SFCertificatePresentation)
new  =
  do
    cls' <- getRequiredClass "SFCertificatePresentation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- presentSheetInWindow:dismissHandler:@
presentSheetInWindow_dismissHandler :: (IsSFCertificatePresentation sfCertificatePresentation, IsNSWindow window) => sfCertificatePresentation -> window -> Ptr () -> IO ()
presentSheetInWindow_dismissHandler sfCertificatePresentation  window dismissHandler =
  withObjCPtr window $ \raw_window ->
      sendMsg sfCertificatePresentation (mkSelector "presentSheetInWindow:dismissHandler:") retVoid [argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr dismissHandler :: Ptr ())]

-- | Dismisses the certificate sheet.
--
-- ObjC selector: @- dismissSheet@
dismissSheet :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO ()
dismissSheet sfCertificatePresentation  =
    sendMsg sfCertificatePresentation (mkSelector "dismissSheet") retVoid []

-- | A trust reference, previously created with SecTrustCreateWithCertificates (see <Security/SecTrust.h>).
--
-- ObjC selector: @- trust@
trust :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Ptr ())
trust sfCertificatePresentation  =
    fmap castPtr $ sendMsg sfCertificatePresentation (mkSelector "trust") (retPtr retVoid) []

-- | Title string to be displayed. If no title is provided, a default title will be used.
--
-- ObjC selector: @- title@
title :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Id NSString)
title sfCertificatePresentation  =
    sendMsg sfCertificatePresentation (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Title string to be displayed. If no title is provided, a default title will be used.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsSFCertificatePresentation sfCertificatePresentation, IsNSString value) => sfCertificatePresentation -> value -> IO ()
setTitle sfCertificatePresentation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg sfCertificatePresentation (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Message string to be displayed. If no message is provided, a default message will be used.
--
-- ObjC selector: @- message@
message :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Id NSString)
message sfCertificatePresentation  =
    sendMsg sfCertificatePresentation (mkSelector "message") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Message string to be displayed. If no message is provided, a default message will be used.
--
-- ObjC selector: @- setMessage:@
setMessage :: (IsSFCertificatePresentation sfCertificatePresentation, IsNSString value) => sfCertificatePresentation -> value -> IO ()
setMessage sfCertificatePresentation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg sfCertificatePresentation (mkSelector "setMessage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The URL that will be opened by clicking the "Learn More" button.
--
-- ObjC selector: @- helpURL@
helpURL :: IsSFCertificatePresentation sfCertificatePresentation => sfCertificatePresentation -> IO (Id NSURL)
helpURL sfCertificatePresentation  =
    sendMsg sfCertificatePresentation (mkSelector "helpURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URL that will be opened by clicking the "Learn More" button.
--
-- ObjC selector: @- setHelpURL:@
setHelpURL :: (IsSFCertificatePresentation sfCertificatePresentation, IsNSURL value) => sfCertificatePresentation -> value -> IO ()
setHelpURL sfCertificatePresentation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg sfCertificatePresentation (mkSelector "setHelpURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTrust:@
initWithTrustSelector :: Selector
initWithTrustSelector = mkSelector "initWithTrust:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @presentSheetInWindow:dismissHandler:@
presentSheetInWindow_dismissHandlerSelector :: Selector
presentSheetInWindow_dismissHandlerSelector = mkSelector "presentSheetInWindow:dismissHandler:"

-- | @Selector@ for @dismissSheet@
dismissSheetSelector :: Selector
dismissSheetSelector = mkSelector "dismissSheet"

-- | @Selector@ for @trust@
trustSelector :: Selector
trustSelector = mkSelector "trust"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @message@
messageSelector :: Selector
messageSelector = mkSelector "message"

-- | @Selector@ for @setMessage:@
setMessageSelector :: Selector
setMessageSelector = mkSelector "setMessage:"

-- | @Selector@ for @helpURL@
helpURLSelector :: Selector
helpURLSelector = mkSelector "helpURL"

-- | @Selector@ for @setHelpURL:@
setHelpURLSelector :: Selector
setHelpURLSelector = mkSelector "setHelpURL:"

