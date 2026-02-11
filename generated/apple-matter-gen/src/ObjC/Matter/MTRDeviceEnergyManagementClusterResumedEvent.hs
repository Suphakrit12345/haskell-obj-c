{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterResumedEvent@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterResumedEvent
  ( MTRDeviceEnergyManagementClusterResumedEvent
  , IsMTRDeviceEnergyManagementClusterResumedEvent(..)
  , cause
  , setCause
  , causeSelector
  , setCauseSelector


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

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterResumedEvent mtrDeviceEnergyManagementClusterResumedEvent => mtrDeviceEnergyManagementClusterResumedEvent -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterResumedEvent  =
    sendMsg mtrDeviceEnergyManagementClusterResumedEvent (mkSelector "cause") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterResumedEvent mtrDeviceEnergyManagementClusterResumedEvent, IsNSNumber value) => mtrDeviceEnergyManagementClusterResumedEvent -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterResumedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterResumedEvent (mkSelector "setCause:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cause@
causeSelector :: Selector
causeSelector = mkSelector "cause"

-- | @Selector@ for @setCause:@
setCauseSelector :: Selector
setCauseSelector = mkSelector "setCause:"

