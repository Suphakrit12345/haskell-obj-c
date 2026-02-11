{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterActiveScheduleChangeEvent@.
module ObjC.Matter.MTRThermostatClusterActiveScheduleChangeEvent
  ( MTRThermostatClusterActiveScheduleChangeEvent
  , IsMTRThermostatClusterActiveScheduleChangeEvent(..)
  , previousScheduleHandle
  , setPreviousScheduleHandle
  , currentScheduleHandle
  , setCurrentScheduleHandle
  , previousScheduleHandleSelector
  , setPreviousScheduleHandleSelector
  , currentScheduleHandleSelector
  , setCurrentScheduleHandleSelector


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

-- | @- previousScheduleHandle@
previousScheduleHandle :: IsMTRThermostatClusterActiveScheduleChangeEvent mtrThermostatClusterActiveScheduleChangeEvent => mtrThermostatClusterActiveScheduleChangeEvent -> IO (Id NSData)
previousScheduleHandle mtrThermostatClusterActiveScheduleChangeEvent  =
    sendMsg mtrThermostatClusterActiveScheduleChangeEvent (mkSelector "previousScheduleHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousScheduleHandle:@
setPreviousScheduleHandle :: (IsMTRThermostatClusterActiveScheduleChangeEvent mtrThermostatClusterActiveScheduleChangeEvent, IsNSData value) => mtrThermostatClusterActiveScheduleChangeEvent -> value -> IO ()
setPreviousScheduleHandle mtrThermostatClusterActiveScheduleChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterActiveScheduleChangeEvent (mkSelector "setPreviousScheduleHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentScheduleHandle@
currentScheduleHandle :: IsMTRThermostatClusterActiveScheduleChangeEvent mtrThermostatClusterActiveScheduleChangeEvent => mtrThermostatClusterActiveScheduleChangeEvent -> IO (Id NSData)
currentScheduleHandle mtrThermostatClusterActiveScheduleChangeEvent  =
    sendMsg mtrThermostatClusterActiveScheduleChangeEvent (mkSelector "currentScheduleHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentScheduleHandle:@
setCurrentScheduleHandle :: (IsMTRThermostatClusterActiveScheduleChangeEvent mtrThermostatClusterActiveScheduleChangeEvent, IsNSData value) => mtrThermostatClusterActiveScheduleChangeEvent -> value -> IO ()
setCurrentScheduleHandle mtrThermostatClusterActiveScheduleChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterActiveScheduleChangeEvent (mkSelector "setCurrentScheduleHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousScheduleHandle@
previousScheduleHandleSelector :: Selector
previousScheduleHandleSelector = mkSelector "previousScheduleHandle"

-- | @Selector@ for @setPreviousScheduleHandle:@
setPreviousScheduleHandleSelector :: Selector
setPreviousScheduleHandleSelector = mkSelector "setPreviousScheduleHandle:"

-- | @Selector@ for @currentScheduleHandle@
currentScheduleHandleSelector :: Selector
currentScheduleHandleSelector = mkSelector "currentScheduleHandle"

-- | @Selector@ for @setCurrentScheduleHandle:@
setCurrentScheduleHandleSelector :: Selector
setCurrentScheduleHandleSelector = mkSelector "setCurrentScheduleHandle:"

