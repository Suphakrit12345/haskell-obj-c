{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A view controller that displays an interface to enable or disable the host app’s extensions.
--
-- When your host app supports app extensions, use this view controller to give people a way to enable or disable those extensions. When you present this view controller, the system displays an out-of-process UI with a list of all app extensions that support your app’s extension points. Someone using your app can use the presented interface to enable or disable extensions selectively. App extensions you include inside your host app’s bundle are enabled by default, but extensions that ship in separate apps are disabled by default.
--
-- Present this view controller modally from your app, or embed the view controller as a child in one of your existing view controller interfaces. For example, you might choose to embed the view controller in a tab of your app’s preferences interface.
--
-- Generated bindings for @EXAppExtensionBrowserViewController@.
module ObjC.ExtensionKit.EXAppExtensionBrowserViewController
  ( EXAppExtensionBrowserViewController
  , IsEXAppExtensionBrowserViewController(..)


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

import ObjC.ExtensionKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

