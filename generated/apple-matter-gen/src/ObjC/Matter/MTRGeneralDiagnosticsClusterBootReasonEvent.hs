{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterBootReasonEvent@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterBootReasonEvent
  ( MTRGeneralDiagnosticsClusterBootReasonEvent
  , IsMTRGeneralDiagnosticsClusterBootReasonEvent(..)
  , bootReason
  , setBootReason
  , bootReasonSelector
  , setBootReasonSelector


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

-- | @- bootReason@
bootReason :: IsMTRGeneralDiagnosticsClusterBootReasonEvent mtrGeneralDiagnosticsClusterBootReasonEvent => mtrGeneralDiagnosticsClusterBootReasonEvent -> IO (Id NSNumber)
bootReason mtrGeneralDiagnosticsClusterBootReasonEvent  =
    sendMsg mtrGeneralDiagnosticsClusterBootReasonEvent (mkSelector "bootReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBootReason:@
setBootReason :: (IsMTRGeneralDiagnosticsClusterBootReasonEvent mtrGeneralDiagnosticsClusterBootReasonEvent, IsNSNumber value) => mtrGeneralDiagnosticsClusterBootReasonEvent -> value -> IO ()
setBootReason mtrGeneralDiagnosticsClusterBootReasonEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterBootReasonEvent (mkSelector "setBootReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bootReason@
bootReasonSelector :: Selector
bootReasonSelector = mkSelector "bootReason"

-- | @Selector@ for @setBootReason:@
setBootReasonSelector :: Selector
setBootReasonSelector = mkSelector "setBootReason:"

