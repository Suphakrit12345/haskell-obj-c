{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A unique identifier for a specific health concept within a domain.
--
-- Each identifier points to one concept inside a domain. For example, within the medication domain, one identifier might represent ibuprofen while another represents insulin.
--
-- Generated bindings for @HKHealthConceptIdentifier@.
module ObjC.HealthKit.HKHealthConceptIdentifier
  ( HKHealthConceptIdentifier
  , IsHKHealthConceptIdentifier(..)
  , init_
  , domain
  , initSelector
  , domainSelector


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
init_ :: IsHKHealthConceptIdentifier hkHealthConceptIdentifier => hkHealthConceptIdentifier -> IO (Id HKHealthConceptIdentifier)
init_ hkHealthConceptIdentifier  =
    sendMsg hkHealthConceptIdentifier (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The domain this identifier belongs to.
--
-- This value identifies the group of concepts the identifier comes from. For example, if the identifier represents a medication, the category will be the medication domain.
--
-- ObjC selector: @- domain@
domain :: IsHKHealthConceptIdentifier hkHealthConceptIdentifier => hkHealthConceptIdentifier -> IO (Id NSString)
domain hkHealthConceptIdentifier  =
    sendMsg hkHealthConceptIdentifier (mkSelector "domain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @domain@
domainSelector :: Selector
domainSelector = mkSelector "domain"

