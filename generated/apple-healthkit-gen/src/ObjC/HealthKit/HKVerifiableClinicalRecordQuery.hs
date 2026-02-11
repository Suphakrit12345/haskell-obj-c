{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKVerifiableClinicalRecordQuery@.
module ObjC.HealthKit.HKVerifiableClinicalRecordQuery
  ( HKVerifiableClinicalRecordQuery
  , IsHKVerifiableClinicalRecordQuery(..)
  , init_
  , new
  , recordTypes
  , sourceTypes
  , initSelector
  , newSelector
  , recordTypesSelector
  , sourceTypesSelector


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
init_ :: IsHKVerifiableClinicalRecordQuery hkVerifiableClinicalRecordQuery => hkVerifiableClinicalRecordQuery -> IO (Id HKVerifiableClinicalRecordQuery)
init_ hkVerifiableClinicalRecordQuery  =
    sendMsg hkVerifiableClinicalRecordQuery (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKVerifiableClinicalRecordQuery)
new  =
  do
    cls' <- getRequiredClass "HKVerifiableClinicalRecordQuery"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | recordTypes
--
-- The record types that need to be present on desired records.
--
-- ObjC selector: @- recordTypes@
recordTypes :: IsHKVerifiableClinicalRecordQuery hkVerifiableClinicalRecordQuery => hkVerifiableClinicalRecordQuery -> IO (Id NSArray)
recordTypes hkVerifiableClinicalRecordQuery  =
    sendMsg hkVerifiableClinicalRecordQuery (mkSelector "recordTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceTypes
--
-- The source type(s) of the records.
--
-- ObjC selector: @- sourceTypes@
sourceTypes :: IsHKVerifiableClinicalRecordQuery hkVerifiableClinicalRecordQuery => hkVerifiableClinicalRecordQuery -> IO (Id NSArray)
sourceTypes hkVerifiableClinicalRecordQuery  =
    sendMsg hkVerifiableClinicalRecordQuery (mkSelector "sourceTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @recordTypes@
recordTypesSelector :: Selector
recordTypesSelector = mkSelector "recordTypes"

-- | @Selector@ for @sourceTypes@
sourceTypesSelector :: Selector
sourceTypesSelector = mkSelector "sourceTypes"

