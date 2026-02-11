{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKBloodTypeObject
--
-- A wrapper object for HKBloodType enumeration.
--
-- Generated bindings for @HKBloodTypeObject@.
module ObjC.HealthKit.HKBloodTypeObject
  ( HKBloodTypeObject
  , IsHKBloodTypeObject(..)
  , bloodType
  , bloodTypeSelector

  -- * Enum types
  , HKBloodType(HKBloodType)
  , pattern HKBloodTypeNotSet
  , pattern HKBloodTypeAPositive
  , pattern HKBloodTypeANegative
  , pattern HKBloodTypeBPositive
  , pattern HKBloodTypeBNegative
  , pattern HKBloodTypeABPositive
  , pattern HKBloodTypeABNegative
  , pattern HKBloodTypeOPositive
  , pattern HKBloodTypeONegative

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

-- | @- bloodType@
bloodType :: IsHKBloodTypeObject hkBloodTypeObject => hkBloodTypeObject -> IO HKBloodType
bloodType hkBloodTypeObject  =
    fmap (coerce :: CLong -> HKBloodType) $ sendMsg hkBloodTypeObject (mkSelector "bloodType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bloodType@
bloodTypeSelector :: Selector
bloodTypeSelector = mkSelector "bloodType"

