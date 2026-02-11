{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterRadioFaultChangeEvent@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterRadioFaultChangeEvent
  ( MTRGeneralDiagnosticsClusterRadioFaultChangeEvent
  , IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent(..)
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
current :: IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent => mtrGeneralDiagnosticsClusterRadioFaultChangeEvent -> IO (Id NSArray)
current mtrGeneralDiagnosticsClusterRadioFaultChangeEvent  =
    sendMsg mtrGeneralDiagnosticsClusterRadioFaultChangeEvent (mkSelector "current") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrent:@
setCurrent :: (IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterRadioFaultChangeEvent -> value -> IO ()
setCurrent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterRadioFaultChangeEvent (mkSelector "setCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previous@
previous :: IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent => mtrGeneralDiagnosticsClusterRadioFaultChangeEvent -> IO (Id NSArray)
previous mtrGeneralDiagnosticsClusterRadioFaultChangeEvent  =
    sendMsg mtrGeneralDiagnosticsClusterRadioFaultChangeEvent (mkSelector "previous") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrevious:@
setPrevious :: (IsMTRGeneralDiagnosticsClusterRadioFaultChangeEvent mtrGeneralDiagnosticsClusterRadioFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterRadioFaultChangeEvent -> value -> IO ()
setPrevious mtrGeneralDiagnosticsClusterRadioFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterRadioFaultChangeEvent (mkSelector "setPrevious:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

