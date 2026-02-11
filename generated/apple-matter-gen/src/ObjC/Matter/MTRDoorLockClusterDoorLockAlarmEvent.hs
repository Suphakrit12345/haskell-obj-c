{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterDoorLockAlarmEvent@.
module ObjC.Matter.MTRDoorLockClusterDoorLockAlarmEvent
  ( MTRDoorLockClusterDoorLockAlarmEvent
  , IsMTRDoorLockClusterDoorLockAlarmEvent(..)
  , alarmCode
  , setAlarmCode
  , alarmCodeSelector
  , setAlarmCodeSelector


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

-- | @- alarmCode@
alarmCode :: IsMTRDoorLockClusterDoorLockAlarmEvent mtrDoorLockClusterDoorLockAlarmEvent => mtrDoorLockClusterDoorLockAlarmEvent -> IO (Id NSNumber)
alarmCode mtrDoorLockClusterDoorLockAlarmEvent  =
    sendMsg mtrDoorLockClusterDoorLockAlarmEvent (mkSelector "alarmCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlarmCode:@
setAlarmCode :: (IsMTRDoorLockClusterDoorLockAlarmEvent mtrDoorLockClusterDoorLockAlarmEvent, IsNSNumber value) => mtrDoorLockClusterDoorLockAlarmEvent -> value -> IO ()
setAlarmCode mtrDoorLockClusterDoorLockAlarmEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterDoorLockAlarmEvent (mkSelector "setAlarmCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmCode@
alarmCodeSelector :: Selector
alarmCodeSelector = mkSelector "alarmCode"

-- | @Selector@ for @setAlarmCode:@
setAlarmCodeSelector :: Selector
setAlarmCodeSelector = mkSelector "setAlarmCode:"

