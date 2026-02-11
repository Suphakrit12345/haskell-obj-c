{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ExtensionKit.Internal.Classes (
    module ObjC.ExtensionKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- EXAppExtensionBrowserViewController ----------

-- | A view controller that displays an interface to enable or disable the host app’s extensions.
--
-- When your host app supports app extensions, use this view controller to give people a way to enable or disable those extensions. When you present this view controller, the system displays an out-of-process UI with a list of all app extensions that support your app’s extension points. Someone using your app can use the presented interface to enable or disable extensions selectively. App extensions you include inside your host app’s bundle are enabled by default, but extensions that ship in separate apps are disabled by default.
--
-- Present this view controller modally from your app, or embed the view controller as a child in one of your existing view controller interfaces. For example, you might choose to embed the view controller in a tab of your app’s preferences interface.
-- 
-- Phantom type for @EXAppExtensionBrowserViewController@.
data EXAppExtensionBrowserViewController

instance IsObjCObject (Id EXAppExtensionBrowserViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EXAppExtensionBrowserViewController"

class IsNSViewController a => IsEXAppExtensionBrowserViewController a where
  toEXAppExtensionBrowserViewController :: a -> Id EXAppExtensionBrowserViewController

instance IsEXAppExtensionBrowserViewController (Id EXAppExtensionBrowserViewController) where
  toEXAppExtensionBrowserViewController = unsafeCastId

instance IsNSObject (Id EXAppExtensionBrowserViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id EXAppExtensionBrowserViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id EXAppExtensionBrowserViewController) where
  toNSViewController = unsafeCastId

-- ---------- EXHostViewController ----------

-- | A view controller that hosts remote views provided by an app extension.
--
-- Present this view controller from your app’s interface to display the content for an associated app extension. Configure the view controller with the app extension identity and the specific scene you want to display. Use the associated delegate object to receive notifications when the app extension becomes active or inactive.
--
-- For more information about presenting this view controller and using it to display an app extension’s UI, see <doc://com.apple.documentation/documentation/extensionkit/including-extension-based-ui-in-your-interface>.
-- 
-- Phantom type for @EXHostViewController@.
data EXHostViewController

instance IsObjCObject (Id EXHostViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EXHostViewController"

class IsNSViewController a => IsEXHostViewController a where
  toEXHostViewController :: a -> Id EXHostViewController

instance IsEXHostViewController (Id EXHostViewController) where
  toEXHostViewController = unsafeCastId

instance IsNSObject (Id EXHostViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id EXHostViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id EXHostViewController) where
  toNSViewController = unsafeCastId
