{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentCloudKitContainer@.
module ObjC.CoreData.NSPersistentCloudKitContainer
  ( NSPersistentCloudKitContainer
  , IsNSPersistentCloudKitContainer(..)
  , initializeCloudKitSchemaWithOptions_error
  , recordForManagedObjectID
  , recordsForManagedObjectIDs
  , recordIDForManagedObjectID
  , recordIDsForManagedObjectIDs
  , canUpdateRecordForManagedObjectWithID
  , canDeleteRecordForManagedObjectWithID
  , canModifyManagedObjectsInStore
  , purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completion
  , persistUpdatedShare_inPersistentStore_completion
  , fetchSharesMatchingObjectIDs_error
  , fetchSharesInPersistentStore_error
  , initializeCloudKitSchemaWithOptions_errorSelector
  , recordForManagedObjectIDSelector
  , recordsForManagedObjectIDsSelector
  , recordIDForManagedObjectIDSelector
  , recordIDsForManagedObjectIDsSelector
  , canUpdateRecordForManagedObjectWithIDSelector
  , canDeleteRecordForManagedObjectWithIDSelector
  , canModifyManagedObjectsInStoreSelector
  , purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completionSelector
  , persistUpdatedShare_inPersistentStore_completionSelector
  , fetchSharesMatchingObjectIDs_errorSelector
  , fetchSharesInPersistentStore_errorSelector

  -- * Enum types
  , NSPersistentCloudKitContainerSchemaInitializationOptions(NSPersistentCloudKitContainerSchemaInitializationOptions)
  , pattern NSPersistentCloudKitContainerSchemaInitializationOptionsNone
  , pattern NSPersistentCloudKitContainerSchemaInitializationOptionsDryRun
  , pattern NSPersistentCloudKitContainerSchemaInitializationOptionsPrintSchema

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

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initializeCloudKitSchemaWithOptions:error:@
initializeCloudKitSchemaWithOptions_error :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSError error_) => nsPersistentCloudKitContainer -> NSPersistentCloudKitContainerSchemaInitializationOptions -> error_ -> IO Bool
initializeCloudKitSchemaWithOptions_error nsPersistentCloudKitContainer  options error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentCloudKitContainer (mkSelector "initializeCloudKitSchemaWithOptions:error:") retCULong [argCULong (coerce options), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- recordForManagedObjectID:@
recordForManagedObjectID :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSManagedObjectID managedObjectID) => nsPersistentCloudKitContainer -> managedObjectID -> IO (Id CKRecord)
recordForManagedObjectID nsPersistentCloudKitContainer  managedObjectID =
  withObjCPtr managedObjectID $ \raw_managedObjectID ->
      sendMsg nsPersistentCloudKitContainer (mkSelector "recordForManagedObjectID:") (retPtr retVoid) [argPtr (castPtr raw_managedObjectID :: Ptr ())] >>= retainedObject . castPtr

-- | @- recordsForManagedObjectIDs:@
recordsForManagedObjectIDs :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSArray managedObjectIDs) => nsPersistentCloudKitContainer -> managedObjectIDs -> IO (Id NSDictionary)
recordsForManagedObjectIDs nsPersistentCloudKitContainer  managedObjectIDs =
  withObjCPtr managedObjectIDs $ \raw_managedObjectIDs ->
      sendMsg nsPersistentCloudKitContainer (mkSelector "recordsForManagedObjectIDs:") (retPtr retVoid) [argPtr (castPtr raw_managedObjectIDs :: Ptr ())] >>= retainedObject . castPtr

-- | @- recordIDForManagedObjectID:@
recordIDForManagedObjectID :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSManagedObjectID managedObjectID) => nsPersistentCloudKitContainer -> managedObjectID -> IO (Id CKRecordID)
recordIDForManagedObjectID nsPersistentCloudKitContainer  managedObjectID =
  withObjCPtr managedObjectID $ \raw_managedObjectID ->
      sendMsg nsPersistentCloudKitContainer (mkSelector "recordIDForManagedObjectID:") (retPtr retVoid) [argPtr (castPtr raw_managedObjectID :: Ptr ())] >>= retainedObject . castPtr

-- | @- recordIDsForManagedObjectIDs:@
recordIDsForManagedObjectIDs :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSArray managedObjectIDs) => nsPersistentCloudKitContainer -> managedObjectIDs -> IO (Id NSDictionary)
recordIDsForManagedObjectIDs nsPersistentCloudKitContainer  managedObjectIDs =
  withObjCPtr managedObjectIDs $ \raw_managedObjectIDs ->
      sendMsg nsPersistentCloudKitContainer (mkSelector "recordIDsForManagedObjectIDs:") (retPtr retVoid) [argPtr (castPtr raw_managedObjectIDs :: Ptr ())] >>= retainedObject . castPtr

-- | @- canUpdateRecordForManagedObjectWithID:@
canUpdateRecordForManagedObjectWithID :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSManagedObjectID objectID) => nsPersistentCloudKitContainer -> objectID -> IO Bool
canUpdateRecordForManagedObjectWithID nsPersistentCloudKitContainer  objectID =
  withObjCPtr objectID $ \raw_objectID ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentCloudKitContainer (mkSelector "canUpdateRecordForManagedObjectWithID:") retCULong [argPtr (castPtr raw_objectID :: Ptr ())]

-- | @- canDeleteRecordForManagedObjectWithID:@
canDeleteRecordForManagedObjectWithID :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSManagedObjectID objectID) => nsPersistentCloudKitContainer -> objectID -> IO Bool
canDeleteRecordForManagedObjectWithID nsPersistentCloudKitContainer  objectID =
  withObjCPtr objectID $ \raw_objectID ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentCloudKitContainer (mkSelector "canDeleteRecordForManagedObjectWithID:") retCULong [argPtr (castPtr raw_objectID :: Ptr ())]

-- | @- canModifyManagedObjectsInStore:@
canModifyManagedObjectsInStore :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSPersistentStore store) => nsPersistentCloudKitContainer -> store -> IO Bool
canModifyManagedObjectsInStore nsPersistentCloudKitContainer  store =
  withObjCPtr store $ \raw_store ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentCloudKitContainer (mkSelector "canModifyManagedObjectsInStore:") retCULong [argPtr (castPtr raw_store :: Ptr ())]

-- | @- purgeObjectsAndRecordsInZoneWithID:inPersistentStore:completion:@
purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completion :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsCKRecordZoneID zoneID, IsNSPersistentStore persistentStore) => nsPersistentCloudKitContainer -> zoneID -> persistentStore -> Ptr () -> IO ()
purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completion nsPersistentCloudKitContainer  zoneID persistentStore completion =
  withObjCPtr zoneID $ \raw_zoneID ->
    withObjCPtr persistentStore $ \raw_persistentStore ->
        sendMsg nsPersistentCloudKitContainer (mkSelector "purgeObjectsAndRecordsInZoneWithID:inPersistentStore:completion:") retVoid [argPtr (castPtr raw_zoneID :: Ptr ()), argPtr (castPtr raw_persistentStore :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- persistUpdatedShare:inPersistentStore:completion:@
persistUpdatedShare_inPersistentStore_completion :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsCKShare share, IsNSPersistentStore persistentStore) => nsPersistentCloudKitContainer -> share -> persistentStore -> Ptr () -> IO ()
persistUpdatedShare_inPersistentStore_completion nsPersistentCloudKitContainer  share persistentStore completion =
  withObjCPtr share $ \raw_share ->
    withObjCPtr persistentStore $ \raw_persistentStore ->
        sendMsg nsPersistentCloudKitContainer (mkSelector "persistUpdatedShare:inPersistentStore:completion:") retVoid [argPtr (castPtr raw_share :: Ptr ()), argPtr (castPtr raw_persistentStore :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- fetchSharesMatchingObjectIDs:error:@
fetchSharesMatchingObjectIDs_error :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSArray objectIDs, IsNSError error_) => nsPersistentCloudKitContainer -> objectIDs -> error_ -> IO (Id NSDictionary)
fetchSharesMatchingObjectIDs_error nsPersistentCloudKitContainer  objectIDs error_ =
  withObjCPtr objectIDs $ \raw_objectIDs ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsPersistentCloudKitContainer (mkSelector "fetchSharesMatchingObjectIDs:error:") (retPtr retVoid) [argPtr (castPtr raw_objectIDs :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- fetchSharesInPersistentStore:error:@
fetchSharesInPersistentStore_error :: (IsNSPersistentCloudKitContainer nsPersistentCloudKitContainer, IsNSPersistentStore persistentStore, IsNSError error_) => nsPersistentCloudKitContainer -> persistentStore -> error_ -> IO (Id NSArray)
fetchSharesInPersistentStore_error nsPersistentCloudKitContainer  persistentStore error_ =
  withObjCPtr persistentStore $ \raw_persistentStore ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsPersistentCloudKitContainer (mkSelector "fetchSharesInPersistentStore:error:") (retPtr retVoid) [argPtr (castPtr raw_persistentStore :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initializeCloudKitSchemaWithOptions:error:@
initializeCloudKitSchemaWithOptions_errorSelector :: Selector
initializeCloudKitSchemaWithOptions_errorSelector = mkSelector "initializeCloudKitSchemaWithOptions:error:"

-- | @Selector@ for @recordForManagedObjectID:@
recordForManagedObjectIDSelector :: Selector
recordForManagedObjectIDSelector = mkSelector "recordForManagedObjectID:"

-- | @Selector@ for @recordsForManagedObjectIDs:@
recordsForManagedObjectIDsSelector :: Selector
recordsForManagedObjectIDsSelector = mkSelector "recordsForManagedObjectIDs:"

-- | @Selector@ for @recordIDForManagedObjectID:@
recordIDForManagedObjectIDSelector :: Selector
recordIDForManagedObjectIDSelector = mkSelector "recordIDForManagedObjectID:"

-- | @Selector@ for @recordIDsForManagedObjectIDs:@
recordIDsForManagedObjectIDsSelector :: Selector
recordIDsForManagedObjectIDsSelector = mkSelector "recordIDsForManagedObjectIDs:"

-- | @Selector@ for @canUpdateRecordForManagedObjectWithID:@
canUpdateRecordForManagedObjectWithIDSelector :: Selector
canUpdateRecordForManagedObjectWithIDSelector = mkSelector "canUpdateRecordForManagedObjectWithID:"

-- | @Selector@ for @canDeleteRecordForManagedObjectWithID:@
canDeleteRecordForManagedObjectWithIDSelector :: Selector
canDeleteRecordForManagedObjectWithIDSelector = mkSelector "canDeleteRecordForManagedObjectWithID:"

-- | @Selector@ for @canModifyManagedObjectsInStore:@
canModifyManagedObjectsInStoreSelector :: Selector
canModifyManagedObjectsInStoreSelector = mkSelector "canModifyManagedObjectsInStore:"

-- | @Selector@ for @purgeObjectsAndRecordsInZoneWithID:inPersistentStore:completion:@
purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completionSelector :: Selector
purgeObjectsAndRecordsInZoneWithID_inPersistentStore_completionSelector = mkSelector "purgeObjectsAndRecordsInZoneWithID:inPersistentStore:completion:"

-- | @Selector@ for @persistUpdatedShare:inPersistentStore:completion:@
persistUpdatedShare_inPersistentStore_completionSelector :: Selector
persistUpdatedShare_inPersistentStore_completionSelector = mkSelector "persistUpdatedShare:inPersistentStore:completion:"

-- | @Selector@ for @fetchSharesMatchingObjectIDs:error:@
fetchSharesMatchingObjectIDs_errorSelector :: Selector
fetchSharesMatchingObjectIDs_errorSelector = mkSelector "fetchSharesMatchingObjectIDs:error:"

-- | @Selector@ for @fetchSharesInPersistentStore:error:@
fetchSharesInPersistentStore_errorSelector :: Selector
fetchSharesInPersistentStore_errorSelector = mkSelector "fetchSharesInPersistentStore:error:"

