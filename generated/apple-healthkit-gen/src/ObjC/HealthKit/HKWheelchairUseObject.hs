{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWheelchairUseObject
--
-- A wrapper object for HKWheelchairUse enumeration.
--
-- Generated bindings for @HKWheelchairUseObject@.
module ObjC.HealthKit.HKWheelchairUseObject
  ( HKWheelchairUseObject
  , IsHKWheelchairUseObject(..)
  , wheelchairUse
  , wheelchairUseSelector

  -- * Enum types
  , HKWheelchairUse(HKWheelchairUse)
  , pattern HKWheelchairUseNotSet
  , pattern HKWheelchairUseNo
  , pattern HKWheelchairUseYes

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

-- | @- wheelchairUse@
wheelchairUse :: IsHKWheelchairUseObject hkWheelchairUseObject => hkWheelchairUseObject -> IO HKWheelchairUse
wheelchairUse hkWheelchairUseObject  =
    fmap (coerce :: CLong -> HKWheelchairUse) $ sendMsg hkWheelchairUseObject (mkSelector "wheelchairUse") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wheelchairUse@
wheelchairUseSelector :: Selector
wheelchairUseSelector = mkSelector "wheelchairUse"

