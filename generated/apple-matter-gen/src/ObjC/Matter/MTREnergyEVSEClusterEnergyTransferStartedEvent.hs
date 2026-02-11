{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEnergyTransferStartedEvent@.
module ObjC.Matter.MTREnergyEVSEClusterEnergyTransferStartedEvent
  ( MTREnergyEVSEClusterEnergyTransferStartedEvent
  , IsMTREnergyEVSEClusterEnergyTransferStartedEvent(..)
  , sessionID
  , setSessionID
  , state
  , setState
  , maximumCurrent
  , setMaximumCurrent
  , maximumDischargeCurrent
  , setMaximumDischargeCurrent
  , sessionIDSelector
  , setSessionIDSelector
  , stateSelector
  , setStateSelector
  , maximumCurrentSelector
  , setMaximumCurrentSelector
  , maximumDischargeCurrentSelector
  , setMaximumDischargeCurrentSelector


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
sessionID :: IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterEnergyTransferStartedEvent  =
    sendMsg mtrEnergyEVSEClusterEnergyTransferStartedEvent (mkSelector "sessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterEnergyTransferStartedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnergyTransferStartedEvent (mkSelector "setSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> IO (Id NSNumber)
state mtrEnergyEVSEClusterEnergyTransferStartedEvent  =
    sendMsg mtrEnergyEVSEClusterEnergyTransferStartedEvent (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setState:@
setState :: (IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> value -> IO ()
setState mtrEnergyEVSEClusterEnergyTransferStartedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnergyTransferStartedEvent (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumCurrent@
maximumCurrent :: IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> IO (Id NSNumber)
maximumCurrent mtrEnergyEVSEClusterEnergyTransferStartedEvent  =
    sendMsg mtrEnergyEVSEClusterEnergyTransferStartedEvent (mkSelector "maximumCurrent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumCurrent:@
setMaximumCurrent :: (IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> value -> IO ()
setMaximumCurrent mtrEnergyEVSEClusterEnergyTransferStartedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnergyTransferStartedEvent (mkSelector "setMaximumCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumDischargeCurrent@
maximumDischargeCurrent :: IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> IO (Id NSNumber)
maximumDischargeCurrent mtrEnergyEVSEClusterEnergyTransferStartedEvent  =
    sendMsg mtrEnergyEVSEClusterEnergyTransferStartedEvent (mkSelector "maximumDischargeCurrent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumDischargeCurrent:@
setMaximumDischargeCurrent :: (IsMTREnergyEVSEClusterEnergyTransferStartedEvent mtrEnergyEVSEClusterEnergyTransferStartedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStartedEvent -> value -> IO ()
setMaximumDischargeCurrent mtrEnergyEVSEClusterEnergyTransferStartedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnergyTransferStartedEvent (mkSelector "setMaximumDischargeCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @setSessionID:@
setSessionIDSelector :: Selector
setSessionIDSelector = mkSelector "setSessionID:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @maximumCurrent@
maximumCurrentSelector :: Selector
maximumCurrentSelector = mkSelector "maximumCurrent"

-- | @Selector@ for @setMaximumCurrent:@
setMaximumCurrentSelector :: Selector
setMaximumCurrentSelector = mkSelector "setMaximumCurrent:"

-- | @Selector@ for @maximumDischargeCurrent@
maximumDischargeCurrentSelector :: Selector
maximumDischargeCurrentSelector = mkSelector "maximumDischargeCurrent"

-- | @Selector@ for @setMaximumDischargeCurrent:@
setMaximumDischargeCurrentSelector :: Selector
setMaximumDischargeCurrentSelector = mkSelector "setMaximumDischargeCurrent:"

