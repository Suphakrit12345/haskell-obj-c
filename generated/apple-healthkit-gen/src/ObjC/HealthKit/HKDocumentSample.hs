{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKDocumentSample
--
-- An abstract class representing a health document.
--
-- Generated bindings for @HKDocumentSample@.
module ObjC.HealthKit.HKDocumentSample
  ( HKDocumentSample
  , IsHKDocumentSample(..)
  , documentType
  , documentTypeSelector


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

-- | @- documentType@
documentType :: IsHKDocumentSample hkDocumentSample => hkDocumentSample -> IO (Id HKDocumentType)
documentType hkDocumentSample  =
    sendMsg hkDocumentSample (mkSelector "documentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @documentType@
documentTypeSelector :: Selector
documentTypeSelector = mkSelector "documentType"

