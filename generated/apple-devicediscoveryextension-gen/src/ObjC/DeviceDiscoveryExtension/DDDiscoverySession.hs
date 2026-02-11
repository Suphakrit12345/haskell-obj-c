{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Manages a session between the extension and host.
--
-- Generated bindings for @DDDiscoverySession@.
module ObjC.DeviceDiscoveryExtension.DDDiscoverySession
  ( DDDiscoverySession
  , IsDDDiscoverySession(..)
  , reportEvent
  , reportEventSelector


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

import ObjC.DeviceDiscoveryExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Reports an event to the host.
--
-- ObjC selector: @- reportEvent:@
reportEvent :: (IsDDDiscoverySession ddDiscoverySession, IsDDDeviceEvent inEvent) => ddDiscoverySession -> inEvent -> IO ()
reportEvent ddDiscoverySession  inEvent =
  withObjCPtr inEvent $ \raw_inEvent ->
      sendMsg ddDiscoverySession (mkSelector "reportEvent:") retVoid [argPtr (castPtr raw_inEvent :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reportEvent:@
reportEventSelector :: Selector
reportEventSelector = mkSelector "reportEvent:"

