{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterBatFaultChangeType@.
module ObjC.Matter.MTRPowerSourceClusterBatFaultChangeType
  ( MTRPowerSourceClusterBatFaultChangeType
  , IsMTRPowerSourceClusterBatFaultChangeType(..)
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
current :: IsMTRPowerSourceClusterBatFaultChangeType mtrPowerSourceClusterBatFaultChangeType => mtrPowerSourceClusterBatFaultChangeType -> IO (Id NSArray)
current mtrPowerSourceClusterBatFaultChangeType  =
    sendMsg mtrPowerSourceClusterBatFaultChangeType (mkSelector "current") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterBatFaultChangeType mtrPowerSourceClusterBatFaultChangeType, IsNSArray value) => mtrPowerSourceClusterBatFaultChangeType -> value -> IO ()
setCurrent mtrPowerSourceClusterBatFaultChangeType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterBatFaultChangeType (mkSelector "setCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previous@
previous :: IsMTRPowerSourceClusterBatFaultChangeType mtrPowerSourceClusterBatFaultChangeType => mtrPowerSourceClusterBatFaultChangeType -> IO (Id NSArray)
previous mtrPowerSourceClusterBatFaultChangeType  =
    sendMsg mtrPowerSourceClusterBatFaultChangeType (mkSelector "previous") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterBatFaultChangeType mtrPowerSourceClusterBatFaultChangeType, IsNSArray value) => mtrPowerSourceClusterBatFaultChangeType -> value -> IO ()
setPrevious mtrPowerSourceClusterBatFaultChangeType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPowerSourceClusterBatFaultChangeType (mkSelector "setPrevious:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

