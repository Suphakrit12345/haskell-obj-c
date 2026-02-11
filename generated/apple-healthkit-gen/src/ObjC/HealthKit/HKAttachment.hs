{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKAttachment
--
-- An HKAttachment represents a file attachment stored in the HealthKit database.
--
-- Generated bindings for @HKAttachment@.
module ObjC.HealthKit.HKAttachment
  ( HKAttachment
  , IsHKAttachment(..)
  , init_
  , new
  , identifier
  , name
  , contentType
  , size
  , creationDate
  , metadata
  , initSelector
  , newSelector
  , identifierSelector
  , nameSelector
  , contentTypeSelector
  , sizeSelector
  , creationDateSelector
  , metadataSelector


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

-- | init
--
-- The init method is unavailable. To create an attachment, use HKAttachmentStore.
--
-- ObjC selector: @- init@
init_ :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id HKAttachment)
init_ hkAttachment  =
    sendMsg hkAttachment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | new
--
-- The new method is unavailable. To create an attachment, use HKAttachmentStore.
--
-- ObjC selector: @+ new@
new :: IO (Id HKAttachment)
new  =
  do
    cls' <- getRequiredClass "HKAttachment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | identifier
--
-- A unique identifier of the receiver in the HealthKit database.
--
-- ObjC selector: @- identifier@
identifier :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id NSUUID)
identifier hkAttachment  =
    sendMsg hkAttachment (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Represents the name of the file.
--
-- ObjC selector: @- name@
name :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id NSString)
name hkAttachment  =
    sendMsg hkAttachment (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | contentType
--
-- The Uniform Type of the file.
--
-- ObjC selector: @- contentType@
contentType :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id UTType)
contentType hkAttachment  =
    sendMsg hkAttachment (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | size
--
-- The size in bytes of the file.
--
-- ObjC selector: @- size@
size :: IsHKAttachment hkAttachment => hkAttachment -> IO CLong
size hkAttachment  =
    sendMsg hkAttachment (mkSelector "size") retCLong []

-- | creationDate
--
-- The date the receiver was created.
--
-- ObjC selector: @- creationDate@
creationDate :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id NSDate)
creationDate hkAttachment  =
    sendMsg hkAttachment (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- Extra information describing the attachment.
--
-- Keys must be NSString and values must be either NSString, NSNumber, or NSDate.
--
-- ObjC selector: @- metadata@
metadata :: IsHKAttachment hkAttachment => hkAttachment -> IO (Id NSDictionary)
metadata hkAttachment  =
    sendMsg hkAttachment (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

