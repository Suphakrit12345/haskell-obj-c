{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterWiredFaultChangeEvent@.
module ObjC.Matter.MTRPowerSourceClusterWiredFaultChangeEvent
  ( MTRPowerSourceClusterWiredFaultChangeEvent
  , IsMTRPowerSourceClusterWiredFaultChangeEvent(..)
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
current :: IsMTRPowerSourceClusterWiredFaultChangeEvent mtrPowerSourceClusterWiredFaultChangeEvent => mtrPowerSourceClusterWiredFaultChangeEvent -> IO (Id NSArray)
current mtrPowerSourceClusterWiredFaultChangeEvent  =
    sendMsg mtrPowerSourceClusterWiredFaultChangeEvent (mkSelector "current") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterWiredFaultChangeEvent mtrPowerSourceClusterWiredFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterWiredFaultChangeEvent -> value -> IO ()
setCurrent mtrPowerSourceClusterWiredFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterWiredFaultChangeEvent (mkSelector "setCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previous@
previous :: IsMTRPowerSourceClusterWiredFaultChangeEvent mtrPowerSourceClusterWiredFaultChangeEvent => mtrPowerSourceClusterWiredFaultChangeEvent -> IO (Id NSArray)
previous mtrPowerSourceClusterWiredFaultChangeEvent  =
    sendMsg mtrPowerSourceClusterWiredFaultChangeEvent (mkSelector "previous") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterWiredFaultChangeEvent mtrPowerSourceClusterWiredFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterWiredFaultChangeEvent -> value -> IO ()
setPrevious mtrPowerSourceClusterWiredFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterWiredFaultChangeEvent (mkSelector "setPrevious:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

