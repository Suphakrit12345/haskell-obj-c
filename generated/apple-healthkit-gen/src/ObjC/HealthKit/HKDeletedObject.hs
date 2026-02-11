{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKDeletedObject
--
-- A class representing an HKObject that was deleted from the HealtKit database.
--
-- Generated bindings for @HKDeletedObject@.
module ObjC.HealthKit.HKDeletedObject
  ( HKDeletedObject
  , IsHKDeletedObject(..)
  , init_
  , uuid
  , metadata
  , initSelector
  , uuidSelector
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

-- | @- init@
init_ :: IsHKDeletedObject hkDeletedObject => hkDeletedObject -> IO (Id HKDeletedObject)
init_ hkDeletedObject  =
    sendMsg hkDeletedObject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | UUID
--
-- The unique identifier of the HKObject that was deleted from the HealthKit database.
--
-- ObjC selector: @- UUID@
uuid :: IsHKDeletedObject hkDeletedObject => hkDeletedObject -> IO (Id NSUUID)
uuid hkDeletedObject  =
    sendMsg hkDeletedObject (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- Extra information describing properties of the receiver.
--
-- Metadata retained from the deleted HKObject.                Available keys: HKMetadataKeySyncIdentifier, HKMetadataKeySyncVersion
--
-- ObjC selector: @- metadata@
metadata :: IsHKDeletedObject hkDeletedObject => hkDeletedObject -> IO (Id NSDictionary)
metadata hkDeletedObject  =
    sendMsg hkDeletedObject (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

