{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterBatFaultChangeEvent@.
module ObjC.Matter.MTRPowerSourceClusterBatFaultChangeEvent
  ( MTRPowerSourceClusterBatFaultChangeEvent
  , IsMTRPowerSourceClusterBatFaultChangeEvent(..)
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
current :: IsMTRPowerSourceClusterBatFaultChangeEvent mtrPowerSourceClusterBatFaultChangeEvent => mtrPowerSourceClusterBatFaultChangeEvent -> IO (Id NSArray)
current mtrPowerSourceClusterBatFaultChangeEvent  =
    sendMsg mtrPowerSourceClusterBatFaultChangeEvent (mkSelector "current") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterBatFaultChangeEvent mtrPowerSourceClusterBatFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterBatFaultChangeEvent -> value -> IO ()
setCurrent mtrPowerSourceClusterBatFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterBatFaultChangeEvent (mkSelector "setCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previous@
previous :: IsMTRPowerSourceClusterBatFaultChangeEvent mtrPowerSourceClusterBatFaultChangeEvent => mtrPowerSourceClusterBatFaultChangeEvent -> IO (Id NSArray)
previous mtrPowerSourceClusterBatFaultChangeEvent  =
    sendMsg mtrPowerSourceClusterBatFaultChangeEvent (mkSelector "previous") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterBatFaultChangeEvent mtrPowerSourceClusterBatFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterBatFaultChangeEvent -> value -> IO ()
setPrevious mtrPowerSourceClusterBatFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterBatFaultChangeEvent (mkSelector "setPrevious:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

