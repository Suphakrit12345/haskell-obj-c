{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSeriesType
--
-- Represents a type of HKSeriesSample
--
-- Generated bindings for @HKSeriesType@.
module ObjC.HealthKit.HKSeriesType
  ( HKSeriesType
  , IsHKSeriesType(..)
  , workoutRouteType
  , heartbeatSeriesType
  , workoutRouteTypeSelector
  , heartbeatSeriesTypeSelector


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

-- | @+ workoutRouteType@
workoutRouteType :: IO (Id HKSeriesType)
workoutRouteType  =
  do
    cls' <- getRequiredClass "HKSeriesType"
    sendClassMsg cls' (mkSelector "workoutRouteType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ heartbeatSeriesType@
heartbeatSeriesType :: IO (Id HKSeriesType)
heartbeatSeriesType  =
  do
    cls' <- getRequiredClass "HKSeriesType"
    sendClassMsg cls' (mkSelector "heartbeatSeriesType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @workoutRouteType@
workoutRouteTypeSelector :: Selector
workoutRouteTypeSelector = mkSelector "workoutRouteType"

-- | @Selector@ for @heartbeatSeriesType@
heartbeatSeriesTypeSelector :: Selector
heartbeatSeriesTypeSelector = mkSelector "heartbeatSeriesType"

