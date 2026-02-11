{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBooleanStateConfigurationClusterSensorFaultEvent@.
module ObjC.Matter.MTRBooleanStateConfigurationClusterSensorFaultEvent
  ( MTRBooleanStateConfigurationClusterSensorFaultEvent
  , IsMTRBooleanStateConfigurationClusterSensorFaultEvent(..)
  , sensorFault
  , setSensorFault
  , sensorFaultSelector
  , setSensorFaultSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sensorFault@
sensorFault :: IsMTRBooleanStateConfigurationClusterSensorFaultEvent mtrBooleanStateConfigurationClusterSensorFaultEvent => mtrBooleanStateConfigurationClusterSensorFaultEvent -> IO (Id NSNumber)
sensorFault mtrBooleanStateConfigurationClusterSensorFaultEvent  =
    sendMsg mtrBooleanStateConfigurationClusterSensorFaultEvent (mkSelector "sensorFault") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSensorFault:@
setSensorFault :: (IsMTRBooleanStateConfigurationClusterSensorFaultEvent mtrBooleanStateConfigurationClusterSensorFaultEvent, IsNSNumber value) => mtrBooleanStateConfigurationClusterSensorFaultEvent -> value -> IO ()
setSensorFault mtrBooleanStateConfigurationClusterSensorFaultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBooleanStateConfigurationClusterSensorFaultEvent (mkSelector "setSensorFault:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sensorFault@
sensorFaultSelector :: Selector
sensorFaultSelector = mkSelector "sensorFault"

-- | @Selector@ for @setSensorFault:@
setSensorFaultSelector :: Selector
setSensorFaultSelector = mkSelector "setSensorFault:"

