{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.GameSave.Internal.Classes (
    module ObjC.GameSave.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- GSSyncedDirectory ----------

-- | A cloud-synced directory for game-save data.
--
-- To get an instance of the directory, call ``openDirectoryForContainerIdentifier:``, which returns the directory for the iCloud container associated with the specified identifier. Calling this method starts syncing the directory in the background on the specified container. When the game needs to access the contents of the directory, show a UI while the directory fully syncs using the ``finishSyncing:completionHandler:`` method. If you're showing your own UI, call the ``finishSyncingWithCompletionHandler:`` method to wait for the directory to finish syncing.
--
-- After the directory is ready to use, syncing pauses until you close the directory object or the object is deallocated. To resume syncing during the game, close and re-open the directory by calling ``close`` and then ``openDirectoryForContainerIdentifier:``.
-- 
-- Phantom type for @GSSyncedDirectory@.
data GSSyncedDirectory

instance IsObjCObject (Id GSSyncedDirectory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GSSyncedDirectory"

class IsNSObject a => IsGSSyncedDirectory a where
  toGSSyncedDirectory :: a -> Id GSSyncedDirectory

instance IsGSSyncedDirectory (Id GSSyncedDirectory) where
  toGSSyncedDirectory = unsafeCastId

instance IsNSObject (Id GSSyncedDirectory) where
  toNSObject = unsafeCastId

-- ---------- GSSyncedDirectoryState ----------

-- | Represents the state and its associated properties of the directory
--
-- Use the ``state`` property to determine the validity of the other properties
-- 
-- Phantom type for @GSSyncedDirectoryState@.
data GSSyncedDirectoryState

instance IsObjCObject (Id GSSyncedDirectoryState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GSSyncedDirectoryState"

class IsNSObject a => IsGSSyncedDirectoryState a where
  toGSSyncedDirectoryState :: a -> Id GSSyncedDirectoryState

instance IsGSSyncedDirectoryState (Id GSSyncedDirectoryState) where
  toGSSyncedDirectoryState = unsafeCastId

instance IsNSObject (Id GSSyncedDirectoryState) where
  toNSObject = unsafeCastId

-- ---------- GSSyncedDirectoryVersion ----------

-- | Phantom type for @GSSyncedDirectoryVersion@.
data GSSyncedDirectoryVersion

instance IsObjCObject (Id GSSyncedDirectoryVersion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GSSyncedDirectoryVersion"

class IsNSObject a => IsGSSyncedDirectoryVersion a where
  toGSSyncedDirectoryVersion :: a -> Id GSSyncedDirectoryVersion

instance IsGSSyncedDirectoryVersion (Id GSSyncedDirectoryVersion) where
  toGSSyncedDirectoryVersion = unsafeCastId

instance IsNSObject (Id GSSyncedDirectoryVersion) where
  toNSObject = unsafeCastId
