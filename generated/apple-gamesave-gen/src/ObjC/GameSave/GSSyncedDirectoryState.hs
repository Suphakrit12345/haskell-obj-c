{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents the state and its associated properties of the directory
--
-- Use the ``state`` property to determine the validity of the other properties
--
-- Generated bindings for @GSSyncedDirectoryState@.
module ObjC.GameSave.GSSyncedDirectoryState
  ( GSSyncedDirectoryState
  , IsGSSyncedDirectoryState(..)
  , init_
  , new
  , state
  , url
  , conflictedVersions
  , error_
  , initSelector
  , newSelector
  , stateSelector
  , urlSelector
  , conflictedVersionsSelector
  , errorSelector

  -- * Enum types
  , GSSyncState(GSSyncState)
  , pattern GSSyncStateReady
  , pattern GSSyncStateOffline
  , pattern GSSyncStateLocal
  , pattern GSSyncStateSyncing
  , pattern GSSyncStateConflicted
  , pattern GSSyncStateError
  , pattern GSSyncStateClosed

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

import ObjC.GameSave.Internal.Classes
import ObjC.GameSave.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id GSSyncedDirectoryState)
init_ gsSyncedDirectoryState  =
    sendMsg gsSyncedDirectoryState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- new@
new :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id GSSyncedDirectoryState)
new gsSyncedDirectoryState  =
    sendMsg gsSyncedDirectoryState (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Specifies the current state of the directory
--
-- ObjC selector: @- state@
state :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO GSSyncState
state gsSyncedDirectoryState  =
    fmap (coerce :: CLong -> GSSyncState) $ sendMsg gsSyncedDirectoryState (mkSelector "state") retCLong []

-- | The URL of a directory to read and write game-save data in.
--
-- This property's value is @nil@ unless the state is @GSSyncStateReady@, @GSSyncStateOffline@, or @GSSyncStateLocal@.
--
-- ObjC selector: @- url@
url :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id NSURL)
url gsSyncedDirectoryState  =
    sendMsg gsSyncedDirectoryState (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The conflicting versions.
--
-- If you're implementing your own conflict resolution, read all of the conflicting versions, and modify one of them to incorporate the state and changes from the others. Then call ``GSSyncedDirectory/resolveConflictsWithVersion:``, passing that version.
--
-- This property's value is @nil@ unless the state is @GSSyncStateConflicted@.
--
-- ObjC selector: @- conflictedVersions@
conflictedVersions :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id NSArray)
conflictedVersions gsSyncedDirectoryState  =
    sendMsg gsSyncedDirectoryState (mkSelector "conflictedVersions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The error preventing you from using the directory.
--
-- This property's value is @nil@ unless the state is @GSSyncStateError@.
--
-- ObjC selector: @- error@
error_ :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id NSError)
error_ gsSyncedDirectoryState  =
    sendMsg gsSyncedDirectoryState (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @conflictedVersions@
conflictedVersionsSelector :: Selector
conflictedVersionsSelector = mkSelector "conflictedVersions"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

