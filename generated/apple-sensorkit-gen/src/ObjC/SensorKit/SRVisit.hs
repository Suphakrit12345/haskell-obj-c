{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRVisit@.
module ObjC.SensorKit.SRVisit
  ( SRVisit
  , IsSRVisit(..)
  , distanceFromHome
  , arrivalDateInterval
  , departureDateInterval
  , locationCategory
  , identifier
  , distanceFromHomeSelector
  , arrivalDateIntervalSelector
  , departureDateIntervalSelector
  , locationCategorySelector
  , identifierSelector

  -- * Enum types
  , SRLocationCategory(SRLocationCategory)
  , pattern SRLocationCategoryUnknown
  , pattern SRLocationCategoryHome
  , pattern SRLocationCategoryWork
  , pattern SRLocationCategorySchool
  , pattern SRLocationCategoryGym

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

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The distance between the location of interest to home
--
-- ObjC selector: @- distanceFromHome@
distanceFromHome :: IsSRVisit srVisit => srVisit -> IO CDouble
distanceFromHome srVisit  =
    sendMsg srVisit (mkSelector "distanceFromHome") retCDouble []

-- | The range of time the arrival to a location of interest occurred
--
-- ObjC selector: @- arrivalDateInterval@
arrivalDateInterval :: IsSRVisit srVisit => srVisit -> IO (Id NSDateInterval)
arrivalDateInterval srVisit  =
    sendMsg srVisit (mkSelector "arrivalDateInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The range of time the departure from a location of interest occurred
--
-- ObjC selector: @- departureDateInterval@
departureDateInterval :: IsSRVisit srVisit => srVisit -> IO (Id NSDateInterval)
departureDateInterval srVisit  =
    sendMsg srVisit (mkSelector "departureDateInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- locationCategory@
locationCategory :: IsSRVisit srVisit => srVisit -> IO SRLocationCategory
locationCategory srVisit  =
    fmap (coerce :: CLong -> SRLocationCategory) $ sendMsg srVisit (mkSelector "locationCategory") retCLong []

-- | An identifier for the location of interest. This can be used to identify the same location regardless of type
--
-- ObjC selector: @- identifier@
identifier :: IsSRVisit srVisit => srVisit -> IO (Id NSUUID)
identifier srVisit  =
    sendMsg srVisit (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @distanceFromHome@
distanceFromHomeSelector :: Selector
distanceFromHomeSelector = mkSelector "distanceFromHome"

-- | @Selector@ for @arrivalDateInterval@
arrivalDateIntervalSelector :: Selector
arrivalDateIntervalSelector = mkSelector "arrivalDateInterval"

-- | @Selector@ for @departureDateInterval@
departureDateIntervalSelector :: Selector
departureDateIntervalSelector = mkSelector "departureDateInterval"

-- | @Selector@ for @locationCategory@
locationCategorySelector :: Selector
locationCategorySelector = mkSelector "locationCategory"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

