{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKAttachmentStore
--
-- The HKAttachmentStore class provides an interface for accessing and storing HKAttachment objects.
--
-- Generated bindings for @HKAttachmentStore@.
module ObjC.HealthKit.HKAttachmentStore
  ( HKAttachmentStore
  , IsHKAttachmentStore(..)
  , initWithHealthStore
  , addAttachmentToObject_name_contentType_URL_metadata_completion
  , removeAttachment_fromObject_completion
  , getDataForAttachment_completion
  , streamDataForAttachment_dataHandler
  , initWithHealthStoreSelector
  , addAttachmentToObject_name_contentType_URL_metadata_completionSelector
  , removeAttachment_fromObject_completionSelector
  , getDataForAttachment_completionSelector
  , streamDataForAttachment_dataHandlerSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | initWithHealthStore:
--
-- The designated initializer to create an HKAttachmentStore.
--
-- @healthStore@ — Specifies the HKHealthStore object to use.
--
-- ObjC selector: @- initWithHealthStore:@
initWithHealthStore :: (IsHKAttachmentStore hkAttachmentStore, IsHKHealthStore healthStore) => hkAttachmentStore -> healthStore -> IO (Id HKAttachmentStore)
initWithHealthStore hkAttachmentStore  healthStore =
  withObjCPtr healthStore $ \raw_healthStore ->
      sendMsg hkAttachmentStore (mkSelector "initWithHealthStore:") (retPtr retVoid) [argPtr (castPtr raw_healthStore :: Ptr ())] >>= ownedObject . castPtr

-- | addAttachmentToObject:name:contentType:URL:metadata:completion:
--
-- Creates a new HKAttachment using the passed in NSURL and attaches it to the specified HKObject.
--
-- @object@ — The object for which to add the HKAttachment.
--
-- @name@ — The name of the attachment.
--
-- @contentType@ — The content type of the attachment.
--
-- @URL@ — The NSURL to use to create the attachment.
--
-- @metadata@ — Extra information describing the attachment.
--
-- @completion@ — Called with an HKAttachment instance once the file was successfully saved and attached,                                otherwise called with an error.
--
-- ObjC selector: @- addAttachmentToObject:name:contentType:URL:metadata:completion:@
addAttachmentToObject_name_contentType_URL_metadata_completion :: (IsHKAttachmentStore hkAttachmentStore, IsHKObject object, IsNSString name, IsUTType contentType, IsNSURL url, IsNSDictionary metadata) => hkAttachmentStore -> object -> name -> contentType -> url -> metadata -> Ptr () -> IO ()
addAttachmentToObject_name_contentType_URL_metadata_completion hkAttachmentStore  object name contentType url metadata completion =
  withObjCPtr object $ \raw_object ->
    withObjCPtr name $ \raw_name ->
      withObjCPtr contentType $ \raw_contentType ->
        withObjCPtr url $ \raw_url ->
          withObjCPtr metadata $ \raw_metadata ->
              sendMsg hkAttachmentStore (mkSelector "addAttachmentToObject:name:contentType:URL:metadata:completion:") retVoid [argPtr (castPtr raw_object :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_contentType :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | removeAttachment:fromObject:completion:
--
-- Removes the given HKAttachment from the specified HKObject.
--
-- @attachment@ — The HKAttachment to be removed.
--
-- @object@ — The object from which to remove the attachment.
--
-- @completion@ — Called once the remove operation finishes.
--
-- ObjC selector: @- removeAttachment:fromObject:completion:@
removeAttachment_fromObject_completion :: (IsHKAttachmentStore hkAttachmentStore, IsHKAttachment attachment, IsHKObject object) => hkAttachmentStore -> attachment -> object -> Ptr () -> IO ()
removeAttachment_fromObject_completion hkAttachmentStore  attachment object completion =
  withObjCPtr attachment $ \raw_attachment ->
    withObjCPtr object $ \raw_object ->
        sendMsg hkAttachmentStore (mkSelector "removeAttachment:fromObject:completion:") retVoid [argPtr (castPtr raw_attachment :: Ptr ()), argPtr (castPtr raw_object :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | getDataForAttachment:completion:
--
-- Retrieves the NSData for the given HKAttachment.
--
-- Prefer @streamDataForAttachment:completion:@ for large files that support incremental reading to limit your app's peak memory usage.                The attachment's data may not always be available locally, and could be stored in iCloud.
--
-- @attachment@ — The attachment object to read data from.
--
-- @completion@ — Called with an NSData or an error.
--
-- Returns: An NSProgress object to use for tracking the progress of downloading the attachment's data from iCloud.
--
-- ObjC selector: @- getDataForAttachment:completion:@
getDataForAttachment_completion :: (IsHKAttachmentStore hkAttachmentStore, IsHKAttachment attachment) => hkAttachmentStore -> attachment -> Ptr () -> IO (Id NSProgress)
getDataForAttachment_completion hkAttachmentStore  attachment completion =
  withObjCPtr attachment $ \raw_attachment ->
      sendMsg hkAttachmentStore (mkSelector "getDataForAttachment:completion:") (retPtr retVoid) [argPtr (castPtr raw_attachment :: Ptr ()), argPtr (castPtr completion :: Ptr ())] >>= retainedObject . castPtr

-- | streamDataForAttachment:dataHandler:
--
-- Streams the given HKAttachment's data as ordered NSData chunks.
--
-- The dataHandler's done parameter is set to YES when all chunks have been streamed.                The attachment's data may not always be available locally, and could be stored in iCloud.
--
-- @attachment@ — The attachment object to read data from.
--
-- @dataHandler@ — Called with an NSData chunk or an error. When done is YES, the operation has completed.
--
-- Returns: An NSProgress object to use for tracking the progress of downloading the attachment's data from iCloud.
--
-- ObjC selector: @- streamDataForAttachment:dataHandler:@
streamDataForAttachment_dataHandler :: (IsHKAttachmentStore hkAttachmentStore, IsHKAttachment attachment) => hkAttachmentStore -> attachment -> Ptr () -> IO (Id NSProgress)
streamDataForAttachment_dataHandler hkAttachmentStore  attachment dataHandler =
  withObjCPtr attachment $ \raw_attachment ->
      sendMsg hkAttachmentStore (mkSelector "streamDataForAttachment:dataHandler:") (retPtr retVoid) [argPtr (castPtr raw_attachment :: Ptr ()), argPtr (castPtr dataHandler :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHealthStore:@
initWithHealthStoreSelector :: Selector
initWithHealthStoreSelector = mkSelector "initWithHealthStore:"

-- | @Selector@ for @addAttachmentToObject:name:contentType:URL:metadata:completion:@
addAttachmentToObject_name_contentType_URL_metadata_completionSelector :: Selector
addAttachmentToObject_name_contentType_URL_metadata_completionSelector = mkSelector "addAttachmentToObject:name:contentType:URL:metadata:completion:"

-- | @Selector@ for @removeAttachment:fromObject:completion:@
removeAttachment_fromObject_completionSelector :: Selector
removeAttachment_fromObject_completionSelector = mkSelector "removeAttachment:fromObject:completion:"

-- | @Selector@ for @getDataForAttachment:completion:@
getDataForAttachment_completionSelector :: Selector
getDataForAttachment_completionSelector = mkSelector "getDataForAttachment:completion:"

-- | @Selector@ for @streamDataForAttachment:dataHandler:@
streamDataForAttachment_dataHandlerSelector :: Selector
streamDataForAttachment_dataHandlerSelector = mkSelector "streamDataForAttachment:dataHandler:"

