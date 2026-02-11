{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A resource that represents an abstract URL.
--
-- An @FSGenericURLResource@ is a completely abstract resource. The only reference to its contents is a single URL, the contents of which are arbitrary. This URL might represent a PCI locator string like `/pci/usb\@5`, or some sort of network address for a remote file system. FSKit leaves interpretation of the URL and its contents entirely up to your implementation.
--
-- Use the @Info.plist@ key @FSSupportedSchemes@ to provide an array of case-insensitive URL schemes that your implementation supports. The following example shows how a hypothetical @FSGenericURLResource@ implementation declares support for the @rsh@ and @ssh@ URL schemes: ``` <key>FSSupportedSchemes</key> <array>     <string>rsh</string>     <string>ssh</string> </array> ```
--
-- Generated bindings for @FSGenericURLResource@.
module ObjC.FSKit.FSGenericURLResource
  ( FSGenericURLResource
  , IsFSGenericURLResource(..)
  , initWithURL
  , init_
  , url
  , initWithURLSelector
  , initSelector
  , urlSelector


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

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a generic URL resource with the given URL. - Parameter url: A URL that provides the content of the file system. The format of this URL is completely arbitrary. It's up to your extension to access the contents represented by the URL and make them available as an ``FSVolume`` that FSKit can load.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsFSGenericURLResource fsGenericURLResource, IsNSURL url) => fsGenericURLResource -> url -> IO (Id FSGenericURLResource)
initWithURL fsGenericURLResource  url =
  withObjCPtr url $ \raw_url ->
      sendMsg fsGenericURLResource (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsFSGenericURLResource fsGenericURLResource => fsGenericURLResource -> IO (Id FSGenericURLResource)
init_ fsGenericURLResource  =
    sendMsg fsGenericURLResource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The URL represented by the resource.
--
-- ObjC selector: @- url@
url :: IsFSGenericURLResource fsGenericURLResource => fsGenericURLResource -> IO (Id NSURL)
url fsGenericURLResource  =
    sendMsg fsGenericURLResource (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

