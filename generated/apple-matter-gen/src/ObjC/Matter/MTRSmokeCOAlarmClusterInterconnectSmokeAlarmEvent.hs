{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent@.
module ObjC.Matter.MTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent
  ( MTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent
  , IsMTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent(..)
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
alarmSeverityLevel :: IsMTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent => mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent -> IO (Id NSNumber)
alarmSeverityLevel mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent  =
    sendMsg mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent (mkSelector "alarmSeverityLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlarmSeverityLevel:@
setAlarmSeverityLevel :: (IsMTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent, IsNSNumber value) => mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent -> value -> IO ()
setAlarmSeverityLevel mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent (mkSelector "setAlarmSeverityLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmSeverityLevel@
alarmSeverityLevelSelector :: Selector
alarmSeverityLevelSelector = mkSelector "alarmSeverityLevel"

-- | @Selector@ for @setAlarmSeverityLevel:@
setAlarmSeverityLevelSelector :: Selector
setAlarmSeverityLevelSelector = mkSelector "setAlarmSeverityLevel:"

