{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterHardwareFaultChangeEvent@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterHardwareFaultChangeEvent
  ( MTRGeneralDiagnosticsClusterHardwareFaultChangeEvent
  , IsMTRGeneralDiagnosticsClusterHardwareFaultChangeEvent(..)
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
current :: IsMTRGeneralDiagnosticsClusterHardwareFaultChangeEvent mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent => mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent -> IO (Id NSArray)
current mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent  =
    sendMsg mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent (mkSelector "current") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrent:@
setCurrent :: (IsMTRGeneralDiagnosticsClusterHardwareFaultChangeEvent mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent -> value -> IO ()
setCurrent mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent (mkSelector "setCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previous@
previous :: IsMTRGeneralDiagnosticsClusterHardwareFaultChangeEvent mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent => mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent -> IO (Id NSArray)
previous mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent  =
    sendMsg mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent (mkSelector "previous") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrevious:@
setPrevious :: (IsMTRGeneralDiagnosticsClusterHardwareFaultChangeEvent mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent -> value -> IO ()
setPrevious mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterHardwareFaultChangeEvent (mkSelector "setPrevious:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

