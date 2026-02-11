{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that represents capabillities supported by a volume, such as hard and symbolic links, journaling, and large file sizes.
--
-- Generated bindings for @FSVolumeSupportedCapabilities@.
module ObjC.FSKit.FSVolumeSupportedCapabilities
  ( FSVolumeSupportedCapabilities
  , IsFSVolumeSupportedCapabilities(..)
  , supportsPersistentObjectIDs
  , setSupportsPersistentObjectIDs
  , supportsSymbolicLinks
  , setSupportsSymbolicLinks
  , supportsHardLinks
  , setSupportsHardLinks
  , supportsJournal
  , setSupportsJournal
  , supportsActiveJournal
  , setSupportsActiveJournal
  , doesNotSupportRootTimes
  , setDoesNotSupportRootTimes
  , supportsSparseFiles
  , setSupportsSparseFiles
  , supportsZeroRuns
  , setSupportsZeroRuns
  , supportsFastStatFS
  , setSupportsFastStatFS
  , supports2TBFiles
  , setSupports2TBFiles
  , supportsOpenDenyModes
  , setSupportsOpenDenyModes
  , supportsHiddenFiles
  , setSupportsHiddenFiles
  , doesNotSupportVolumeSizes
  , setDoesNotSupportVolumeSizes
  , supports64BitObjectIDs
  , setSupports64BitObjectIDs
  , supportsDocumentID
  , setSupportsDocumentID
  , doesNotSupportImmutableFiles
  , setDoesNotSupportImmutableFiles
  , doesNotSupportSettingFilePermissions
  , setDoesNotSupportSettingFilePermissions
  , supportsSharedSpace
  , setSupportsSharedSpace
  , supportsVolumeGroups
  , setSupportsVolumeGroups
  , caseFormat
  , setCaseFormat
  , supportsPersistentObjectIDsSelector
  , setSupportsPersistentObjectIDsSelector
  , supportsSymbolicLinksSelector
  , setSupportsSymbolicLinksSelector
  , supportsHardLinksSelector
  , setSupportsHardLinksSelector
  , supportsJournalSelector
  , setSupportsJournalSelector
  , supportsActiveJournalSelector
  , setSupportsActiveJournalSelector
  , doesNotSupportRootTimesSelector
  , setDoesNotSupportRootTimesSelector
  , supportsSparseFilesSelector
  , setSupportsSparseFilesSelector
  , supportsZeroRunsSelector
  , setSupportsZeroRunsSelector
  , supportsFastStatFSSelector
  , setSupportsFastStatFSSelector
  , supports2TBFilesSelector
  , setSupports2TBFilesSelector
  , supportsOpenDenyModesSelector
  , setSupportsOpenDenyModesSelector
  , supportsHiddenFilesSelector
  , setSupportsHiddenFilesSelector
  , doesNotSupportVolumeSizesSelector
  , setDoesNotSupportVolumeSizesSelector
  , supports64BitObjectIDsSelector
  , setSupports64BitObjectIDsSelector
  , supportsDocumentIDSelector
  , setSupportsDocumentIDSelector
  , doesNotSupportImmutableFilesSelector
  , setDoesNotSupportImmutableFilesSelector
  , doesNotSupportSettingFilePermissionsSelector
  , setDoesNotSupportSettingFilePermissionsSelector
  , supportsSharedSpaceSelector
  , setSupportsSharedSpaceSelector
  , supportsVolumeGroupsSelector
  , setSupportsVolumeGroupsSelector
  , caseFormatSelector
  , setCaseFormatSelector

  -- * Enum types
  , FSVolumeCaseFormat(FSVolumeCaseFormat)
  , pattern FSVolumeCaseFormatSensitive
  , pattern FSVolumeCaseFormatInsensitive
  , pattern FSVolumeCaseFormatInsensitiveCasePreserving

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
import ObjC.FSKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | A Boolean property that indicates whether the volume supports persistent object identifiers and can look up file system objects by their IDs.
--
-- ObjC selector: @- supportsPersistentObjectIDs@
supportsPersistentObjectIDs :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsPersistentObjectIDs fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsPersistentObjectIDs") retCULong []

-- | A Boolean property that indicates whether the volume supports persistent object identifiers and can look up file system objects by their IDs.
--
-- ObjC selector: @- setSupportsPersistentObjectIDs:@
setSupportsPersistentObjectIDs :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsPersistentObjectIDs fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsPersistentObjectIDs:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports symbolic links.
--
-- ObjC selector: @- supportsSymbolicLinks@
supportsSymbolicLinks :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsSymbolicLinks fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsSymbolicLinks") retCULong []

-- | A Boolean property that indicates whether the volume supports symbolic links.
--
-- ObjC selector: @- setSupportsSymbolicLinks:@
setSupportsSymbolicLinks :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsSymbolicLinks fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsSymbolicLinks:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports hard links.
--
-- ObjC selector: @- supportsHardLinks@
supportsHardLinks :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsHardLinks fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsHardLinks") retCULong []

-- | A Boolean property that indicates whether the volume supports hard links.
--
-- ObjC selector: @- setSupportsHardLinks:@
setSupportsHardLinks :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsHardLinks fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsHardLinks:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports a journal used to speed recovery in case of unplanned restart, such as a power outage or crash.
--
-- This property doesn't necessarily mean the volume is actively using a journal.
--
-- ObjC selector: @- supportsJournal@
supportsJournal :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsJournal fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsJournal") retCULong []

-- | A Boolean property that indicates whether the volume supports a journal used to speed recovery in case of unplanned restart, such as a power outage or crash.
--
-- This property doesn't necessarily mean the volume is actively using a journal.
--
-- ObjC selector: @- setSupportsJournal:@
setSupportsJournal :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsJournal fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsJournal:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume currently uses a journal for speeding recovery after an unplanned shutdown.
--
-- ObjC selector: @- supportsActiveJournal@
supportsActiveJournal :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsActiveJournal fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsActiveJournal") retCULong []

-- | A Boolean property that indicates whether the volume currently uses a journal for speeding recovery after an unplanned shutdown.
--
-- ObjC selector: @- setSupportsActiveJournal:@
setSupportsActiveJournal :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsActiveJournal fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsActiveJournal:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolan property that indicates the volume doesn't store reliable times for the root directory.
--
-- If this value is @true@ (Swift) or @YES@ (Objective-C), the volume doesn't store reliable times for the root directory.
--
-- ObjC selector: @- doesNotSupportRootTimes@
doesNotSupportRootTimes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
doesNotSupportRootTimes fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "doesNotSupportRootTimes") retCULong []

-- | A Boolan property that indicates the volume doesn't store reliable times for the root directory.
--
-- If this value is @true@ (Swift) or @YES@ (Objective-C), the volume doesn't store reliable times for the root directory.
--
-- ObjC selector: @- setDoesNotSupportRootTimes:@
setDoesNotSupportRootTimes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setDoesNotSupportRootTimes fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setDoesNotSupportRootTimes:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports sparse files.
--
-- A sparse file is a file that can have "holes" that the file system has never written to, and as a result don't consume space on disk.
--
-- ObjC selector: @- supportsSparseFiles@
supportsSparseFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsSparseFiles fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsSparseFiles") retCULong []

-- | A Boolean property that indicates whether the volume supports sparse files.
--
-- A sparse file is a file that can have "holes" that the file system has never written to, and as a result don't consume space on disk.
--
-- ObjC selector: @- setSupportsSparseFiles:@
setSupportsSparseFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsSparseFiles fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsSparseFiles:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports zero runs
--
-- If this value is true, the volume keeps track of allocated but unwritten runs of a file so that it can substitute zeroes without actually writing zeroes to the media.
--
-- ObjC selector: @- supportsZeroRuns@
supportsZeroRuns :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsZeroRuns fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsZeroRuns") retCULong []

-- | A Boolean property that indicates whether the volume supports zero runs
--
-- If this value is true, the volume keeps track of allocated but unwritten runs of a file so that it can substitute zeroes without actually writing zeroes to the media.
--
-- ObjC selector: @- setSupportsZeroRuns:@
setSupportsZeroRuns :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsZeroRuns fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsZeroRuns:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports fast results when fetching file system statistics.
--
-- A true value means this volume hints to upper layers to indicate that @statfs(2)@ is fast enough that its results need not be cached by the caller.
--
-- ObjC selector: @- supportsFastStatFS@
supportsFastStatFS :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsFastStatFS fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsFastStatFS") retCULong []

-- | A Boolean property that indicates whether the volume supports fast results when fetching file system statistics.
--
-- A true value means this volume hints to upper layers to indicate that @statfs(2)@ is fast enough that its results need not be cached by the caller.
--
-- ObjC selector: @- setSupportsFastStatFS:@
setSupportsFastStatFS :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsFastStatFS fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsFastStatFS:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports file sizes larger than 4GB, and potentially up to 2TB.
--
-- ObjC selector: @- supports2TBFiles@
supports2TBFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supports2TBFiles fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supports2TBFiles") retCULong []

-- | A Boolean property that indicates whether the volume supports file sizes larger than 4GB, and potentially up to 2TB.
--
-- ObjC selector: @- setSupports2TBFiles:@
setSupports2TBFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupports2TBFiles fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupports2TBFiles:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports open deny modes.
--
-- These are modes such as "open for read write, deny write".
--
-- ObjC selector: @- supportsOpenDenyModes@
supportsOpenDenyModes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsOpenDenyModes fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsOpenDenyModes") retCULong []

-- | A Boolean property that indicates whether the volume supports open deny modes.
--
-- These are modes such as "open for read write, deny write".
--
-- ObjC selector: @- setSupportsOpenDenyModes:@
setSupportsOpenDenyModes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsOpenDenyModes fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsOpenDenyModes:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports hidden files.
--
-- A @true@ value means the volume supports the @UF_HIDDEN@ file flag.
--
-- ObjC selector: @- supportsHiddenFiles@
supportsHiddenFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsHiddenFiles fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsHiddenFiles") retCULong []

-- | A Boolean property that indicates whether the volume supports hidden files.
--
-- A @true@ value means the volume supports the @UF_HIDDEN@ file flag.
--
-- ObjC selector: @- setSupportsHiddenFiles:@
setSupportsHiddenFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsHiddenFiles fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsHiddenFiles:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates the volume doesn't support certain volume size reports.
--
-- A true value means the volume doesn't support determining values for total data blocks, available blocks, or free blocks, as in @f_blocks@, @f_bavail@, and @f_bfree@ in the struct @statFS@ returned by @statfs(2)@.
--
-- ObjC selector: @- doesNotSupportVolumeSizes@
doesNotSupportVolumeSizes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
doesNotSupportVolumeSizes fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "doesNotSupportVolumeSizes") retCULong []

-- | A Boolean property that indicates the volume doesn't support certain volume size reports.
--
-- A true value means the volume doesn't support determining values for total data blocks, available blocks, or free blocks, as in @f_blocks@, @f_bavail@, and @f_bfree@ in the struct @statFS@ returned by @statfs(2)@.
--
-- ObjC selector: @- setDoesNotSupportVolumeSizes:@
setDoesNotSupportVolumeSizes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setDoesNotSupportVolumeSizes fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setDoesNotSupportVolumeSizes:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports 64-bit object IDs.
--
-- ObjC selector: @- supports64BitObjectIDs@
supports64BitObjectIDs :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supports64BitObjectIDs fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supports64BitObjectIDs") retCULong []

-- | A Boolean property that indicates whether the volume supports 64-bit object IDs.
--
-- ObjC selector: @- setSupports64BitObjectIDs:@
setSupports64BitObjectIDs :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupports64BitObjectIDs fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupports64BitObjectIDs:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports document IDs for document revisions.
--
-- A document ID is an identifier that persists across object ID changes.
--
-- ObjC selector: @- supportsDocumentID@
supportsDocumentID :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsDocumentID fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsDocumentID") retCULong []

-- | A Boolean property that indicates whether the volume supports document IDs for document revisions.
--
-- A document ID is an identifier that persists across object ID changes.
--
-- ObjC selector: @- setSupportsDocumentID:@
setSupportsDocumentID :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsDocumentID fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsDocumentID:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates the volume doesn't support immutable files.
--
-- A @true@ value means this volume doesn't support setting the @UF_IMMUTABLE@ flag.
--
-- ObjC selector: @- doesNotSupportImmutableFiles@
doesNotSupportImmutableFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
doesNotSupportImmutableFiles fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "doesNotSupportImmutableFiles") retCULong []

-- | A Boolean property that indicates the volume doesn't support immutable files.
--
-- A @true@ value means this volume doesn't support setting the @UF_IMMUTABLE@ flag.
--
-- ObjC selector: @- setDoesNotSupportImmutableFiles:@
setDoesNotSupportImmutableFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setDoesNotSupportImmutableFiles fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setDoesNotSupportImmutableFiles:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates the volume doesn't set file permissions.
--
-- If this value is @true@ (Swift) or @YES@ (Objective-C), the volume doesn't support setting file permissions.
--
-- ObjC selector: @- doesNotSupportSettingFilePermissions@
doesNotSupportSettingFilePermissions :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
doesNotSupportSettingFilePermissions fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "doesNotSupportSettingFilePermissions") retCULong []

-- | A Boolean property that indicates the volume doesn't set file permissions.
--
-- If this value is @true@ (Swift) or @YES@ (Objective-C), the volume doesn't support setting file permissions.
--
-- ObjC selector: @- setDoesNotSupportSettingFilePermissions:@
setDoesNotSupportSettingFilePermissions :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setDoesNotSupportSettingFilePermissions fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setDoesNotSupportSettingFilePermissions:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports multiple logical file systems that share space in a single "partition."
--
-- ObjC selector: @- supportsSharedSpace@
supportsSharedSpace :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsSharedSpace fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsSharedSpace") retCULong []

-- | A Boolean property that indicates whether the volume supports multiple logical file systems that share space in a single "partition."
--
-- ObjC selector: @- setSupportsSharedSpace:@
setSupportsSharedSpace :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsSharedSpace fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsSharedSpace:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean property that indicates whether the volume supports volume groups.
--
-- Volume groups involve multiple logical file systems that the system can mount and unmount together, and for which the system can present common file system identifier information.
--
-- ObjC selector: @- supportsVolumeGroups@
supportsVolumeGroups :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsVolumeGroups fsVolumeSupportedCapabilities  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "supportsVolumeGroups") retCULong []

-- | A Boolean property that indicates whether the volume supports volume groups.
--
-- Volume groups involve multiple logical file systems that the system can mount and unmount together, and for which the system can present common file system identifier information.
--
-- ObjC selector: @- setSupportsVolumeGroups:@
setSupportsVolumeGroups :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsVolumeGroups fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setSupportsVolumeGroups:") retVoid [argCULong (if value then 1 else 0)]

-- | A value that indicates the volume's support for case sensitivity.
--
-- ObjC selector: @- caseFormat@
caseFormat :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO FSVolumeCaseFormat
caseFormat fsVolumeSupportedCapabilities  =
    fmap (coerce :: CLong -> FSVolumeCaseFormat) $ sendMsg fsVolumeSupportedCapabilities (mkSelector "caseFormat") retCLong []

-- | A value that indicates the volume's support for case sensitivity.
--
-- ObjC selector: @- setCaseFormat:@
setCaseFormat :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> FSVolumeCaseFormat -> IO ()
setCaseFormat fsVolumeSupportedCapabilities  value =
    sendMsg fsVolumeSupportedCapabilities (mkSelector "setCaseFormat:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportsPersistentObjectIDs@
supportsPersistentObjectIDsSelector :: Selector
supportsPersistentObjectIDsSelector = mkSelector "supportsPersistentObjectIDs"

-- | @Selector@ for @setSupportsPersistentObjectIDs:@
setSupportsPersistentObjectIDsSelector :: Selector
setSupportsPersistentObjectIDsSelector = mkSelector "setSupportsPersistentObjectIDs:"

-- | @Selector@ for @supportsSymbolicLinks@
supportsSymbolicLinksSelector :: Selector
supportsSymbolicLinksSelector = mkSelector "supportsSymbolicLinks"

-- | @Selector@ for @setSupportsSymbolicLinks:@
setSupportsSymbolicLinksSelector :: Selector
setSupportsSymbolicLinksSelector = mkSelector "setSupportsSymbolicLinks:"

-- | @Selector@ for @supportsHardLinks@
supportsHardLinksSelector :: Selector
supportsHardLinksSelector = mkSelector "supportsHardLinks"

-- | @Selector@ for @setSupportsHardLinks:@
setSupportsHardLinksSelector :: Selector
setSupportsHardLinksSelector = mkSelector "setSupportsHardLinks:"

-- | @Selector@ for @supportsJournal@
supportsJournalSelector :: Selector
supportsJournalSelector = mkSelector "supportsJournal"

-- | @Selector@ for @setSupportsJournal:@
setSupportsJournalSelector :: Selector
setSupportsJournalSelector = mkSelector "setSupportsJournal:"

-- | @Selector@ for @supportsActiveJournal@
supportsActiveJournalSelector :: Selector
supportsActiveJournalSelector = mkSelector "supportsActiveJournal"

-- | @Selector@ for @setSupportsActiveJournal:@
setSupportsActiveJournalSelector :: Selector
setSupportsActiveJournalSelector = mkSelector "setSupportsActiveJournal:"

-- | @Selector@ for @doesNotSupportRootTimes@
doesNotSupportRootTimesSelector :: Selector
doesNotSupportRootTimesSelector = mkSelector "doesNotSupportRootTimes"

-- | @Selector@ for @setDoesNotSupportRootTimes:@
setDoesNotSupportRootTimesSelector :: Selector
setDoesNotSupportRootTimesSelector = mkSelector "setDoesNotSupportRootTimes:"

-- | @Selector@ for @supportsSparseFiles@
supportsSparseFilesSelector :: Selector
supportsSparseFilesSelector = mkSelector "supportsSparseFiles"

-- | @Selector@ for @setSupportsSparseFiles:@
setSupportsSparseFilesSelector :: Selector
setSupportsSparseFilesSelector = mkSelector "setSupportsSparseFiles:"

-- | @Selector@ for @supportsZeroRuns@
supportsZeroRunsSelector :: Selector
supportsZeroRunsSelector = mkSelector "supportsZeroRuns"

-- | @Selector@ for @setSupportsZeroRuns:@
setSupportsZeroRunsSelector :: Selector
setSupportsZeroRunsSelector = mkSelector "setSupportsZeroRuns:"

-- | @Selector@ for @supportsFastStatFS@
supportsFastStatFSSelector :: Selector
supportsFastStatFSSelector = mkSelector "supportsFastStatFS"

-- | @Selector@ for @setSupportsFastStatFS:@
setSupportsFastStatFSSelector :: Selector
setSupportsFastStatFSSelector = mkSelector "setSupportsFastStatFS:"

-- | @Selector@ for @supports2TBFiles@
supports2TBFilesSelector :: Selector
supports2TBFilesSelector = mkSelector "supports2TBFiles"

-- | @Selector@ for @setSupports2TBFiles:@
setSupports2TBFilesSelector :: Selector
setSupports2TBFilesSelector = mkSelector "setSupports2TBFiles:"

-- | @Selector@ for @supportsOpenDenyModes@
supportsOpenDenyModesSelector :: Selector
supportsOpenDenyModesSelector = mkSelector "supportsOpenDenyModes"

-- | @Selector@ for @setSupportsOpenDenyModes:@
setSupportsOpenDenyModesSelector :: Selector
setSupportsOpenDenyModesSelector = mkSelector "setSupportsOpenDenyModes:"

-- | @Selector@ for @supportsHiddenFiles@
supportsHiddenFilesSelector :: Selector
supportsHiddenFilesSelector = mkSelector "supportsHiddenFiles"

-- | @Selector@ for @setSupportsHiddenFiles:@
setSupportsHiddenFilesSelector :: Selector
setSupportsHiddenFilesSelector = mkSelector "setSupportsHiddenFiles:"

-- | @Selector@ for @doesNotSupportVolumeSizes@
doesNotSupportVolumeSizesSelector :: Selector
doesNotSupportVolumeSizesSelector = mkSelector "doesNotSupportVolumeSizes"

-- | @Selector@ for @setDoesNotSupportVolumeSizes:@
setDoesNotSupportVolumeSizesSelector :: Selector
setDoesNotSupportVolumeSizesSelector = mkSelector "setDoesNotSupportVolumeSizes:"

-- | @Selector@ for @supports64BitObjectIDs@
supports64BitObjectIDsSelector :: Selector
supports64BitObjectIDsSelector = mkSelector "supports64BitObjectIDs"

-- | @Selector@ for @setSupports64BitObjectIDs:@
setSupports64BitObjectIDsSelector :: Selector
setSupports64BitObjectIDsSelector = mkSelector "setSupports64BitObjectIDs:"

-- | @Selector@ for @supportsDocumentID@
supportsDocumentIDSelector :: Selector
supportsDocumentIDSelector = mkSelector "supportsDocumentID"

-- | @Selector@ for @setSupportsDocumentID:@
setSupportsDocumentIDSelector :: Selector
setSupportsDocumentIDSelector = mkSelector "setSupportsDocumentID:"

-- | @Selector@ for @doesNotSupportImmutableFiles@
doesNotSupportImmutableFilesSelector :: Selector
doesNotSupportImmutableFilesSelector = mkSelector "doesNotSupportImmutableFiles"

-- | @Selector@ for @setDoesNotSupportImmutableFiles:@
setDoesNotSupportImmutableFilesSelector :: Selector
setDoesNotSupportImmutableFilesSelector = mkSelector "setDoesNotSupportImmutableFiles:"

-- | @Selector@ for @doesNotSupportSettingFilePermissions@
doesNotSupportSettingFilePermissionsSelector :: Selector
doesNotSupportSettingFilePermissionsSelector = mkSelector "doesNotSupportSettingFilePermissions"

-- | @Selector@ for @setDoesNotSupportSettingFilePermissions:@
setDoesNotSupportSettingFilePermissionsSelector :: Selector
setDoesNotSupportSettingFilePermissionsSelector = mkSelector "setDoesNotSupportSettingFilePermissions:"

-- | @Selector@ for @supportsSharedSpace@
supportsSharedSpaceSelector :: Selector
supportsSharedSpaceSelector = mkSelector "supportsSharedSpace"

-- | @Selector@ for @setSupportsSharedSpace:@
setSupportsSharedSpaceSelector :: Selector
setSupportsSharedSpaceSelector = mkSelector "setSupportsSharedSpace:"

-- | @Selector@ for @supportsVolumeGroups@
supportsVolumeGroupsSelector :: Selector
supportsVolumeGroupsSelector = mkSelector "supportsVolumeGroups"

-- | @Selector@ for @setSupportsVolumeGroups:@
setSupportsVolumeGroupsSelector :: Selector
setSupportsVolumeGroupsSelector = mkSelector "setSupportsVolumeGroups:"

-- | @Selector@ for @caseFormat@
caseFormatSelector :: Selector
caseFormatSelector = mkSelector "caseFormat"

-- | @Selector@ for @setCaseFormat:@
setCaseFormatSelector :: Selector
setCaseFormatSelector = mkSelector "setCaseFormat:"

