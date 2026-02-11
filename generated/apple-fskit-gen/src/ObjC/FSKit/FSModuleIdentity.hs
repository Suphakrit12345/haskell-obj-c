{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An installed file system module.
--
-- Generated bindings for @FSModuleIdentity@.
module ObjC.FSKit.FSModuleIdentity
  ( FSModuleIdentity
  , IsFSModuleIdentity(..)
  , bundleIdentifier
  , url
  , enabled
  , bundleIdentifierSelector
  , urlSelector
  , enabledSelector


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

-- | The module's bundle identifier.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsFSModuleIdentity fsModuleIdentity => fsModuleIdentity -> IO (Id NSString)
bundleIdentifier fsModuleIdentity  =
    sendMsg fsModuleIdentity (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The module's URL.
--
-- ObjC selector: @- url@
url :: IsFSModuleIdentity fsModuleIdentity => fsModuleIdentity -> IO (Id NSURL)
url fsModuleIdentity  =
    sendMsg fsModuleIdentity (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value that indicates if the module is enabled.
--
-- ObjC selector: @- enabled@
enabled :: IsFSModuleIdentity fsModuleIdentity => fsModuleIdentity -> IO Bool
enabled fsModuleIdentity  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsModuleIdentity (mkSelector "enabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

