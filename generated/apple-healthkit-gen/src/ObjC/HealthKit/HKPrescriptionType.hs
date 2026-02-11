{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKPrescriptionType
--
-- Represents a prescription type
--
-- Generated bindings for @HKPrescriptionType@.
module ObjC.HealthKit.HKPrescriptionType
  ( HKPrescriptionType
  , IsHKPrescriptionType(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

