{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A cloud-synced directory for game-save data.
--
-- To get an instance of the directory, call ``openDirectoryForContainerIdentifier:``, which returns the directory for the iCloud container associated with the specified identifier. Calling this method starts syncing the directory in the background on the specified container. When the game needs to access the contents of the directory, show a UI while the directory fully syncs using the ``finishSyncing:completionHandler:`` method. If you're showing your own UI, call the ``finishSyncingWithCompletionHandler:`` method to wait for the directory to finish syncing.
--
-- After the directory is ready to use, syncing pauses until you close the directory object or the object is deallocated. To resume syncing during the game, close and re-open the directory by calling ``close`` and then ``openDirectoryForContainerIdentifier:``.
--
-- Generated bindings for @GSSyncedDirectory@.
module ObjC.GameSave.GSSyncedDirectory
  ( GSSyncedDirectory
  , IsGSSyncedDirectory(..)
  , openDirectoryForContainerIdentifier
  , close
  , triggerPendingUploadWithCompletionHandler
  , resolveConflictsWithVersion
  , finishSyncingWithCompletionHandler
  , finishSyncing_completionHandler
  , init_
  , new
  , directoryState
  , openDirectoryForContainerIdentifierSelector
  , closeSelector
  , triggerPendingUploadWithCompletionHandlerSelector
  , resolveConflictsWithVersionSelector
  , finishSyncingWithCompletionHandlerSelector
  , finishSyncing_completionHandlerSelector
  , initSelector
  , newSelector
  , directoryStateSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Requests an instance of the game-save directory.
--
-- - Parameter containerIdentifier: The identifier of the directory to request.   If you pass @nil@, this method uses the first container identifier   listed in the @com.apple.developer.icloud-container-identifiers@ entitlements array.
--
-- This method returns immediately, and starts syncing the directory in the background. To wait for syncing to complete, call the ``finishSyncingWithCompletionHandler:`` method.
--
-- ObjC selector: @+ openDirectoryForContainerIdentifier:@
openDirectoryForContainerIdentifier :: IsNSString containerIdentifier => containerIdentifier -> IO (Id GSSyncedDirectory)
openDirectoryForContainerIdentifier containerIdentifier =
  do
    cls' <- getRequiredClass "GSSyncedDirectory"
    withObjCPtr containerIdentifier $ \raw_containerIdentifier ->
      sendClassMsg cls' (mkSelector "openDirectoryForContainerIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_containerIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | Closes the directory, and resumes syncing the directory to the cloud.
--
-- ObjC selector: @- close@
close :: IsGSSyncedDirectory gsSyncedDirectory => gsSyncedDirectory -> IO ()
close gsSyncedDirectory  =
    sendMsg gsSyncedDirectory (mkSelector "close") retVoid []

-- | Triggers an upload of the directory for any changes that were pending.
--
-- Calls the completion block with @YES@ if there were pending uploads; otherwise with @NO@.
--
-- ObjC selector: @- triggerPendingUploadWithCompletionHandler:@
triggerPendingUploadWithCompletionHandler :: IsGSSyncedDirectory gsSyncedDirectory => gsSyncedDirectory -> Ptr () -> IO ()
triggerPendingUploadWithCompletionHandler gsSyncedDirectory  completion =
    sendMsg gsSyncedDirectory (mkSelector "triggerPendingUploadWithCompletionHandler:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Indicates that you resolved a conflict.
--
-- - Parameter version: The version to use.
--
-- If you're implementing your own conflict resolution, read all of the conflicting versions, and modify one of them to incorporate the state and changes from the others. Then call this method, passing that version.
--
-- Call this method only when the directory is in the ``GSSyncState/GSSyncStateConflicted`` state.
--
-- ObjC selector: @- resolveConflictsWithVersion:@
resolveConflictsWithVersion :: (IsGSSyncedDirectory gsSyncedDirectory, IsGSSyncedDirectoryVersion version) => gsSyncedDirectory -> version -> IO ()
resolveConflictsWithVersion gsSyncedDirectory  version =
  withObjCPtr version $ \raw_version ->
      sendMsg gsSyncedDirectory (mkSelector "resolveConflictsWithVersion:") retVoid [argPtr (castPtr raw_version :: Ptr ())]

-- | Waits for the directory sync to complete, without showing any user interface.
--
-- Use this method to wait if your app displays its own syncing UI.
--
-- ObjC selector: @- finishSyncingWithCompletionHandler:@
finishSyncingWithCompletionHandler :: IsGSSyncedDirectory gsSyncedDirectory => gsSyncedDirectory -> Ptr () -> IO ()
finishSyncingWithCompletionHandler gsSyncedDirectory  completion =
    sendMsg gsSyncedDirectory (mkSelector "finishSyncingWithCompletionHandler:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Waits for the directory sync to complete, showing the sync's progress in a modal alert.
--
-- - Parameters:   - statusDisplay: The window where the system shows progress and alerts.   - completion: The block that GameSave calls after syncing finishes.
--
-- If the sync results in a conflict, the framework displays a conflict resolution UI for the user to chose a version that will be used. If the user isn't signed in to iCloud or iCloud drive, the framework informs the user and then switches to local saving.
--
-- ObjC selector: @- finishSyncing:completionHandler:@
finishSyncing_completionHandler :: (IsGSSyncedDirectory gsSyncedDirectory, IsNSWindow statusDisplay) => gsSyncedDirectory -> statusDisplay -> Ptr () -> IO ()
finishSyncing_completionHandler gsSyncedDirectory  statusDisplay completion =
  withObjCPtr statusDisplay $ \raw_statusDisplay ->
      sendMsg gsSyncedDirectory (mkSelector "finishSyncing:completionHandler:") retVoid [argPtr (castPtr raw_statusDisplay :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsGSSyncedDirectory gsSyncedDirectory => gsSyncedDirectory -> IO (Id GSSyncedDirectory)
init_ gsSyncedDirectory  =
    sendMsg gsSyncedDirectory (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- new@
new :: IsGSSyncedDirectory gsSyncedDirectory => gsSyncedDirectory -> IO (Id GSSyncedDirectory)
new gsSyncedDirectory  =
    sendMsg gsSyncedDirectory (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The state of the directory.
--
-- ObjC selector: @- directoryState@
directoryState :: IsGSSyncedDirectory gsSyncedDirectory => gsSyncedDirectory -> IO (Id GSSyncedDirectoryState)
directoryState gsSyncedDirectory  =
    sendMsg gsSyncedDirectory (mkSelector "directoryState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openDirectoryForContainerIdentifier:@
openDirectoryForContainerIdentifierSelector :: Selector
openDirectoryForContainerIdentifierSelector = mkSelector "openDirectoryForContainerIdentifier:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @triggerPendingUploadWithCompletionHandler:@
triggerPendingUploadWithCompletionHandlerSelector :: Selector
triggerPendingUploadWithCompletionHandlerSelector = mkSelector "triggerPendingUploadWithCompletionHandler:"

-- | @Selector@ for @resolveConflictsWithVersion:@
resolveConflictsWithVersionSelector :: Selector
resolveConflictsWithVersionSelector = mkSelector "resolveConflictsWithVersion:"

-- | @Selector@ for @finishSyncingWithCompletionHandler:@
finishSyncingWithCompletionHandlerSelector :: Selector
finishSyncingWithCompletionHandlerSelector = mkSelector "finishSyncingWithCompletionHandler:"

-- | @Selector@ for @finishSyncing:completionHandler:@
finishSyncing_completionHandlerSelector :: Selector
finishSyncing_completionHandlerSelector = mkSelector "finishSyncing:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @directoryState@
directoryStateSelector :: Selector
directoryStateSelector = mkSelector "directoryState"

