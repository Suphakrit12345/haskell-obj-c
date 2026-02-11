{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterNetworkFaultChangeEvent@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterNetworkFaultChangeEvent
  ( MTRGeneralDiagnosticsClusterNetworkFaultChangeEvent
  , IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent(..)
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
current :: IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent => mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent -> IO (Id NSArray)
current mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent (mkSelector "current") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrent:@
setCurrent :: (IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent -> value -> IO ()
setCurrent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent (mkSelector "setCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previous@
previous :: IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent => mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent -> IO (Id NSArray)
previous mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent (mkSelector "previous") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrevious:@
setPrevious :: (IsMTRGeneralDiagnosticsClusterNetworkFaultChangeEvent mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent -> value -> IO ()
setPrevious mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkFaultChangeEvent (mkSelector "setPrevious:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

