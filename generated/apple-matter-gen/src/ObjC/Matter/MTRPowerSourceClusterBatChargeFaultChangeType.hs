{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterBatChargeFaultChangeType@.
module ObjC.Matter.MTRPowerSourceClusterBatChargeFaultChangeType
  ( MTRPowerSourceClusterBatChargeFaultChangeType
  , IsMTRPowerSourceClusterBatChargeFaultChangeType(..)
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
current :: IsMTRPowerSourceClusterBatChargeFaultChangeType mtrPowerSourceClusterBatChargeFaultChangeType => mtrPowerSourceClusterBatChargeFaultChangeType -> IO (Id NSArray)
current mtrPowerSourceClusterBatChargeFaultChangeType  =
    sendMsg mtrPowerSourceClusterBatChargeFaultChangeType (mkSelector "current") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterBatChargeFaultChangeType mtrPowerSourceClusterBatChargeFaultChangeType, IsNSArray value) => mtrPowerSourceClusterBatChargeFaultChangeType -> value -> IO ()
setCurrent mtrPowerSourceClusterBatChargeFaultChangeType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterBatChargeFaultChangeType (mkSelector "setCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previous@
previous :: IsMTRPowerSourceClusterBatChargeFaultChangeType mtrPowerSourceClusterBatChargeFaultChangeType => mtrPowerSourceClusterBatChargeFaultChangeType -> IO (Id NSArray)
previous mtrPowerSourceClusterBatChargeFaultChangeType  =
    sendMsg mtrPowerSourceClusterBatChargeFaultChangeType (mkSelector "previous") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterBatChargeFaultChangeType mtrPowerSourceClusterBatChargeFaultChangeType, IsNSArray value) => mtrPowerSourceClusterBatChargeFaultChangeType -> value -> IO ()
setPrevious mtrPowerSourceClusterBatChargeFaultChangeType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterBatChargeFaultChangeType (mkSelector "setPrevious:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

