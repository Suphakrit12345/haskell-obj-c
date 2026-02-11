{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEVConnectedEvent@.
module ObjC.Matter.MTREnergyEVSEClusterEVConnectedEvent
  ( MTREnergyEVSEClusterEVConnectedEvent
  , IsMTREnergyEVSEClusterEVConnectedEvent(..)
  , sessionID
  , setSessionID
  , sessionIDSelector
  , setSessionIDSelector


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

-- | @- sessionID@
sessionID :: IsMTREnergyEVSEClusterEVConnectedEvent mtrEnergyEVSEClusterEVConnectedEvent => mtrEnergyEVSEClusterEVConnectedEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterEVConnectedEvent  =
    sendMsg mtrEnergyEVSEClusterEVConnectedEvent (mkSelector "sessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterEVConnectedEvent mtrEnergyEVSEClusterEVConnectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVConnectedEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterEVConnectedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEVConnectedEvent (mkSelector "setSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @setSessionID:@
setSessionIDSelector :: Selector
setSessionIDSelector = mkSelector "setSessionID:"

