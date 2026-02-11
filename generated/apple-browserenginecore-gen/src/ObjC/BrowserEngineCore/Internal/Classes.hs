{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.BrowserEngineCore.Internal.Classes (
    module ObjC.BrowserEngineCore.Internal.Classes,
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- BEAudioSession ----------

-- | An object that represents an audio session
-- 
-- Phantom type for @BEAudioSession@.
data BEAudioSession

instance IsObjCObject (Id BEAudioSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEAudioSession"

class IsNSObject a => IsBEAudioSession a where
  toBEAudioSession :: a -> Id BEAudioSession

instance IsBEAudioSession (Id BEAudioSession) where
  toBEAudioSession = unsafeCastId

instance IsNSObject (Id BEAudioSession) where
  toNSObject = unsafeCastId
