{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterWiredFaultChangeType@.
module ObjC.Matter.MTRPowerSourceClusterWiredFaultChangeType
  ( MTRPowerSourceClusterWiredFaultChangeType
  , IsMTRPowerSourceClusterWiredFaultChangeType(..)
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
current :: IsMTRPowerSourceClusterWiredFaultChangeType mtrPowerSourceClusterWiredFaultChangeType => mtrPowerSourceClusterWiredFaultChangeType -> IO (Id NSArray)
current mtrPowerSourceClusterWiredFaultChangeType  =
    sendMsg mtrPowerSourceClusterWiredFaultChangeType (mkSelector "current") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterWiredFaultChangeType mtrPowerSourceClusterWiredFaultChangeType, IsNSArray value) => mtrPowerSourceClusterWiredFaultChangeType -> value -> IO ()
setCurrent mtrPowerSourceClusterWiredFaultChangeType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterWiredFaultChangeType (mkSelector "setCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previous@
previous :: IsMTRPowerSourceClusterWiredFaultChangeType mtrPowerSourceClusterWiredFaultChangeType => mtrPowerSourceClusterWiredFaultChangeType -> IO (Id NSArray)
previous mtrPowerSourceClusterWiredFaultChangeType  =
    sendMsg mtrPowerSourceClusterWiredFaultChangeType (mkSelector "previous") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterWiredFaultChangeType mtrPowerSourceClusterWiredFaultChangeType, IsNSArray value) => mtrPowerSourceClusterWiredFaultChangeType -> value -> IO ()
setPrevious mtrPowerSourceClusterWiredFaultChangeType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterWiredFaultChangeType (mkSelector "setPrevious:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

