{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterRFIDEvent@.
module ObjC.Matter.MTREnergyEVSEClusterRFIDEvent
  ( MTREnergyEVSEClusterRFIDEvent
  , IsMTREnergyEVSEClusterRFIDEvent(..)
  , uid
  , setUid
  , uidSelector
  , setUidSelector


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

-- | @- uid@
uid :: IsMTREnergyEVSEClusterRFIDEvent mtrEnergyEVSEClusterRFIDEvent => mtrEnergyEVSEClusterRFIDEvent -> IO (Id NSData)
uid mtrEnergyEVSEClusterRFIDEvent  =
    sendMsg mtrEnergyEVSEClusterRFIDEvent (mkSelector "uid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUid:@
setUid :: (IsMTREnergyEVSEClusterRFIDEvent mtrEnergyEVSEClusterRFIDEvent, IsNSData value) => mtrEnergyEVSEClusterRFIDEvent -> value -> IO ()
setUid mtrEnergyEVSEClusterRFIDEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterRFIDEvent (mkSelector "setUid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uid@
uidSelector :: Selector
uidSelector = mkSelector "uid"

-- | @Selector@ for @setUid:@
setUidSelector :: Selector
setUidSelector = mkSelector "setUid:"

