{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSString@.
module ObjC.SensorKit.NSString
  ( NSString
  , IsNSString(..)
  , sr_sensorForDeletionRecordsFromSensor
  , sr_sensorForDeletionRecordsFromSensorSelector


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
import ObjC.Foundation.Internal.Classes
import Data.String (IsString(..))
import ObjC.Runtime.NSString (pureNSString)

-- | Returns a sensor stream that contains deletion records of the sensor
--
-- This sensor stream should only be used for fetching. All other operations will be ignored. Deletion records share the recording and authorization state with their parent sensor.
--
-- Returns: May return nil if there is no deletion record available for this sensor
--
-- ObjC selector: @- sr_sensorForDeletionRecordsFromSensor@
sr_sensorForDeletionRecordsFromSensor :: IsNSString nsString => nsString -> IO (Id NSString)
sr_sensorForDeletionRecordsFromSensor nsString  =
    sendMsg nsString (mkSelector "sr_sensorForDeletionRecordsFromSensor") (retPtr retVoid) [] >>= retainedObject . castPtr


-- | Allows using @OverloadedStrings@ for @Id NSString@.
--
-- >>> :set -XOverloadedStrings
-- >>> let s = "hello" :: Id NSString
instance IsString (Id NSString) where
  fromString = pureNSString
-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sr_sensorForDeletionRecordsFromSensor@
sr_sensorForDeletionRecordsFromSensorSelector :: Selector
sr_sensorForDeletionRecordsFromSensorSelector = mkSelector "sr_sensorForDeletionRecordsFromSensor"

