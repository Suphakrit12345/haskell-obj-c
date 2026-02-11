{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterBatChargeFaultChangeEvent@.
module ObjC.Matter.MTRPowerSourceClusterBatChargeFaultChangeEvent
  ( MTRPowerSourceClusterBatChargeFaultChangeEvent
  , IsMTRPowerSourceClusterBatChargeFaultChangeEvent(..)
  , current
  , setCurrent
  , previous
  , setPrevious
  , currentSelector
  , setCurrentSelector
  , previousSelector
  , setPreviousSelector


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

-- | @- current@
current :: IsMTRPowerSourceClusterBatChargeFaultChangeEvent mtrPowerSourceClusterBatChargeFaultChangeEvent => mtrPowerSourceClusterBatChargeFaultChangeEvent -> IO (Id NSArray)
current mtrPowerSourceClusterBatChargeFaultChangeEvent  =
    sendMsg mtrPowerSourceClusterBatChargeFaultChangeEvent (mkSelector "current") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterBatChargeFaultChangeEvent mtrPowerSourceClusterBatChargeFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterBatChargeFaultChangeEvent -> value -> IO ()
setCurrent mtrPowerSourceClusterBatChargeFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterBatChargeFaultChangeEvent (mkSelector "setCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previous@
previous :: IsMTRPowerSourceClusterBatChargeFaultChangeEvent mtrPowerSourceClusterBatChargeFaultChangeEvent => mtrPowerSourceClusterBatChargeFaultChangeEvent -> IO (Id NSArray)
previous mtrPowerSourceClusterBatChargeFaultChangeEvent  =
    sendMsg mtrPowerSourceClusterBatChargeFaultChangeEvent (mkSelector "previous") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterBatChargeFaultChangeEvent mtrPowerSourceClusterBatChargeFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterBatChargeFaultChangeEvent -> value -> IO ()
setPrevious mtrPowerSourceClusterBatChargeFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterBatChargeFaultChangeEvent (mkSelector "setPrevious:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @current@
currentSelector :: Selector
currentSelector = mkSelector "current"

-- | @Selector@ for @setCurrent:@
setCurrentSelector :: Selector
setCurrentSelector = mkSelector "setCurrent:"

-- | @Selector@ for @previous@
previousSelector :: Selector
previousSelector = mkSelector "previous"

-- | @Selector@ for @setPrevious:@
setPreviousSelector :: Selector
setPreviousSelector = mkSelector "setPrevious:"

