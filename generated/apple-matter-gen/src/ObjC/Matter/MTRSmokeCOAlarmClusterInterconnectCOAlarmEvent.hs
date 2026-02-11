{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSmokeCOAlarmClusterInterconnectCOAlarmEvent@.
module ObjC.Matter.MTRSmokeCOAlarmClusterInterconnectCOAlarmEvent
  ( MTRSmokeCOAlarmClusterInterconnectCOAlarmEvent
  , IsMTRSmokeCOAlarmClusterInterconnectCOAlarmEvent(..)
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
alarmSeverityLevel :: IsMTRSmokeCOAlarmClusterInterconnectCOAlarmEvent mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent => mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent -> IO (Id NSNumber)
alarmSeverityLevel mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent  =
    sendMsg mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent (mkSelector "alarmSeverityLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlarmSeverityLevel:@
setAlarmSeverityLevel :: (IsMTRSmokeCOAlarmClusterInterconnectCOAlarmEvent mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent, IsNSNumber value) => mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent -> value -> IO ()
setAlarmSeverityLevel mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent (mkSelector "setAlarmSeverityLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmSeverityLevel@
alarmSeverityLevelSelector :: Selector
alarmSeverityLevelSelector = mkSelector "alarmSeverityLevel"

-- | @Selector@ for @setAlarmSeverityLevel:@
setAlarmSeverityLevelSelector :: Selector
setAlarmSeverityLevelSelector = mkSelector "setAlarmSeverityLevel:"

