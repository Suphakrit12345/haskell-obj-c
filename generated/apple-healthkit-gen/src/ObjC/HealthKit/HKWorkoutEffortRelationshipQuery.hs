{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkoutEffortRelationshipQuery
--
-- A concrete subclass of HKQuery that provides an interface to observe associations with a workout sample.
--
-- Generated bindings for @HKWorkoutEffortRelationshipQuery@.
module ObjC.HealthKit.HKWorkoutEffortRelationshipQuery
  ( HKWorkoutEffortRelationshipQuery
  , IsHKWorkoutEffortRelationshipQuery(..)

  -- * Enum types
  , HKWorkoutEffortRelationshipQueryOptions(HKWorkoutEffortRelationshipQueryOptions)
  , pattern HKWorkoutEffortRelationshipQueryOptionsDefault
  , pattern HKWorkoutEffortRelationshipQueryOptionsMostRelevant

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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

