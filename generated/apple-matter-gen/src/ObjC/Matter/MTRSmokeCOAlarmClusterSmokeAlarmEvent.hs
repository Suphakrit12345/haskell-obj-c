{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSmokeCOAlarmClusterSmokeAlarmEvent@.
module ObjC.Matter.MTRSmokeCOAlarmClusterSmokeAlarmEvent
  ( MTRSmokeCOAlarmClusterSmokeAlarmEvent
  , IsMTRSmokeCOAlarmClusterSmokeAlarmEvent(..)
  , alarmSeverityLevel
  , setAlarmSeverityLevel
  , alarmSeverityLevelSelector
  , setAlarmSeverityLevelSelector


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

-- | @- alarmSeverityLevel@
alarmSeverityLevel :: IsMTRSmokeCOAlarmClusterSmokeAlarmEvent mtrSmokeCOAlarmClusterSmokeAlarmEvent => mtrSmokeCOAlarmClusterSmokeAlarmEvent -> IO (Id NSNumber)
alarmSeverityLevel mtrSmokeCOAlarmClusterSmokeAlarmEvent  =
    sendMsg mtrSmokeCOAlarmClusterSmokeAlarmEvent (mkSelector "alarmSeverityLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlarmSeverityLevel:@
setAlarmSeverityLevel :: (IsMTRSmokeCOAlarmClusterSmokeAlarmEvent mtrSmokeCOAlarmClusterSmokeAlarmEvent, IsNSNumber value) => mtrSmokeCOAlarmClusterSmokeAlarmEvent -> value -> IO ()
setAlarmSeverityLevel mtrSmokeCOAlarmClusterSmokeAlarmEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSmokeCOAlarmClusterSmokeAlarmEvent (mkSelector "setAlarmSeverityLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmSeverityLevel@
alarmSeverityLevelSelector :: Selector
alarmSeverityLevelSelector = mkSelector "alarmSeverityLevel"

-- | @Selector@ for @setAlarmSeverityLevel:@
setAlarmSeverityLevelSelector :: Selector
setAlarmSeverityLevelSelector = mkSelector "setAlarmSeverityLevel:"

