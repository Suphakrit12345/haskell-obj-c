{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKBiologicalSexObject
--
-- A wrapper object for HKBiologicalSex enumeration.
--
-- Generated bindings for @HKBiologicalSexObject@.
module ObjC.HealthKit.HKBiologicalSexObject
  ( HKBiologicalSexObject
  , IsHKBiologicalSexObject(..)
  , biologicalSex
  , biologicalSexSelector

  -- * Enum types
  , HKBiologicalSex(HKBiologicalSex)
  , pattern HKBiologicalSexNotSet
  , pattern HKBiologicalSexFemale
  , pattern HKBiologicalSexMale
  , pattern HKBiologicalSexOther

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
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- biologicalSex@
biologicalSex :: IsHKBiologicalSexObject hkBiologicalSexObject => hkBiologicalSexObject -> IO HKBiologicalSex
biologicalSex hkBiologicalSexObject  =
    fmap (coerce :: CLong -> HKBiologicalSex) $ sendMsg hkBiologicalSexObject (mkSelector "biologicalSex") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @biologicalSex@
biologicalSexSelector :: Selector
biologicalSexSelector = mkSelector "biologicalSex"

