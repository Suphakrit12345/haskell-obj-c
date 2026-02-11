{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKDocumentQuery
--
-- A concrete subclass of HKQuery that provides an interface to retrieve documents from the Health store.
--
-- Generated bindings for @HKDocumentQuery@.
module ObjC.HealthKit.HKDocumentQuery
  ( HKDocumentQuery
  , IsHKDocumentQuery(..)
  , limit
  , sortDescriptors
  , includeDocumentData
  , limitSelector
  , sortDescriptorsSelector
  , includeDocumentDataSelector


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

-- | limit
--
-- The maximum number of documents the receiver will return upon completion.
--
-- ObjC selector: @- limit@
limit :: IsHKDocumentQuery hkDocumentQuery => hkDocumentQuery -> IO CULong
limit hkDocumentQuery  =
    sendMsg hkDocumentQuery (mkSelector "limit") retCULong []

-- | sortDescriptors
--
-- An array of NSSortDescriptors.
--
-- ObjC selector: @- sortDescriptors@
sortDescriptors :: IsHKDocumentQuery hkDocumentQuery => hkDocumentQuery -> IO (Id NSArray)
sortDescriptors hkDocumentQuery  =
    sendMsg hkDocumentQuery (mkSelector "sortDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | includeDocumentData
--
-- The XML content for documents may be large.  This property can be used to control whether the query                returns the XML content for each record.
--
-- ObjC selector: @- includeDocumentData@
includeDocumentData :: IsHKDocumentQuery hkDocumentQuery => hkDocumentQuery -> IO Bool
includeDocumentData hkDocumentQuery  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkDocumentQuery (mkSelector "includeDocumentData") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @limit@
limitSelector :: Selector
limitSelector = mkSelector "limit"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @includeDocumentData@
includeDocumentDataSelector :: Selector
includeDocumentDataSelector = mkSelector "includeDocumentData"

