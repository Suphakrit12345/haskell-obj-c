{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKActivityMoveModeObject
--
-- A wrapper object for HKActivityMoveMode enumeration.
--
-- Generated bindings for @HKActivityMoveModeObject@.
module ObjC.HealthKit.HKActivityMoveModeObject
  ( HKActivityMoveModeObject
  , IsHKActivityMoveModeObject(..)
  , activityMoveMode
  , activityMoveModeSelector

  -- * Enum types
  , HKActivityMoveMode(HKActivityMoveMode)
  , pattern HKActivityMoveModeActiveEnergy
  , pattern HKActivityMoveModeAppleMoveTime

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

-- | @- activityMoveMode@
activityMoveMode :: IsHKActivityMoveModeObject hkActivityMoveModeObject => hkActivityMoveModeObject -> IO HKActivityMoveMode
activityMoveMode hkActivityMoveModeObject  =
    fmap (coerce :: CLong -> HKActivityMoveMode) $ sendMsg hkActivityMoveModeObject (mkSelector "activityMoveMode") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activityMoveMode@
activityMoveModeSelector :: Selector
activityMoveModeSelector = mkSelector "activityMoveMode"

