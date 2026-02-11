{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A resource that represents a path in the system file space.
--
-- The URL passed to @FSPathURLResource@ may be a security-scoped URL. If the URL is a security-scoped URL, FSKit transports it intact from a client application to your extension.
--
-- Generated bindings for @FSPathURLResource@.
module ObjC.FSKit.FSPathURLResource
  ( FSPathURLResource
  , IsFSPathURLResource(..)
  , initWithURL_writable
  , init_
  , url
  , writable
  , initWithURL_writableSelector
  , initSelector
  , urlSelector
  , writableSelector


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

-- | Creates a path URL resource. - Parameters:   - URL: A URL in the system file space that represents the contents of a file system. This parameter uses the @file:@ scheme.   - writable: A Boolean value that indicates whether the file system supports writing to the contents of the URL.
--
-- ObjC selector: @- initWithURL:writable:@
initWithURL_writable :: (IsFSPathURLResource fsPathURLResource, IsNSURL url) => fsPathURLResource -> url -> Bool -> IO (Id FSPathURLResource)
initWithURL_writable fsPathURLResource  url writable =
  withObjCPtr url $ \raw_url ->
      sendMsg fsPathURLResource (mkSelector "initWithURL:writable:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if writable then 1 else 0)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsFSPathURLResource fsPathURLResource => fsPathURLResource -> IO (Id FSPathURLResource)
init_ fsPathURLResource  =
    sendMsg fsPathURLResource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The URL represented by the resource.
--
-- ObjC selector: @- url@
url :: IsFSPathURLResource fsPathURLResource => fsPathURLResource -> IO (Id NSURL)
url fsPathURLResource  =
    sendMsg fsPathURLResource (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value that indicates whether the file system supports writing to the contents of the path URL.
--
-- ObjC selector: @- writable@
writable :: IsFSPathURLResource fsPathURLResource => fsPathURLResource -> IO Bool
writable fsPathURLResource  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsPathURLResource (mkSelector "writable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:writable:@
initWithURL_writableSelector :: Selector
initWithURL_writableSelector = mkSelector "initWithURL:writable:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @writable@
writableSelector :: Selector
writableSelector = mkSelector "writable"

