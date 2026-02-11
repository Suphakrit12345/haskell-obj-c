{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralCommissioningClusterBasicCommissioningInfo@.
module ObjC.Matter.MTRGeneralCommissioningClusterBasicCommissioningInfo
  ( MTRGeneralCommissioningClusterBasicCommissioningInfo
  , IsMTRGeneralCommissioningClusterBasicCommissioningInfo(..)
  , failSafeExpiryLengthSeconds
  , setFailSafeExpiryLengthSeconds
  , maxCumulativeFailsafeSeconds
  , setMaxCumulativeFailsafeSeconds
  , failSafeExpiryLengthSecondsSelector
  , setFailSafeExpiryLengthSecondsSelector
  , maxCumulativeFailsafeSecondsSelector
  , setMaxCumulativeFailsafeSecondsSelector


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

-- | @- failSafeExpiryLengthSeconds@
failSafeExpiryLengthSeconds :: IsMTRGeneralCommissioningClusterBasicCommissioningInfo mtrGeneralCommissioningClusterBasicCommissioningInfo => mtrGeneralCommissioningClusterBasicCommissioningInfo -> IO (Id NSNumber)
failSafeExpiryLengthSeconds mtrGeneralCommissioningClusterBasicCommissioningInfo  =
    sendMsg mtrGeneralCommissioningClusterBasicCommissioningInfo (mkSelector "failSafeExpiryLengthSeconds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFailSafeExpiryLengthSeconds:@
setFailSafeExpiryLengthSeconds :: (IsMTRGeneralCommissioningClusterBasicCommissioningInfo mtrGeneralCommissioningClusterBasicCommissioningInfo, IsNSNumber value) => mtrGeneralCommissioningClusterBasicCommissioningInfo -> value -> IO ()
setFailSafeExpiryLengthSeconds mtrGeneralCommissioningClusterBasicCommissioningInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterBasicCommissioningInfo (mkSelector "setFailSafeExpiryLengthSeconds:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxCumulativeFailsafeSeconds@
maxCumulativeFailsafeSeconds :: IsMTRGeneralCommissioningClusterBasicCommissioningInfo mtrGeneralCommissioningClusterBasicCommissioningInfo => mtrGeneralCommissioningClusterBasicCommissioningInfo -> IO (Id NSNumber)
maxCumulativeFailsafeSeconds mtrGeneralCommissioningClusterBasicCommissioningInfo  =
    sendMsg mtrGeneralCommissioningClusterBasicCommissioningInfo (mkSelector "maxCumulativeFailsafeSeconds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxCumulativeFailsafeSeconds:@
setMaxCumulativeFailsafeSeconds :: (IsMTRGeneralCommissioningClusterBasicCommissioningInfo mtrGeneralCommissioningClusterBasicCommissioningInfo, IsNSNumber value) => mtrGeneralCommissioningClusterBasicCommissioningInfo -> value -> IO ()
setMaxCumulativeFailsafeSeconds mtrGeneralCommissioningClusterBasicCommissioningInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterBasicCommissioningInfo (mkSelector "setMaxCumulativeFailsafeSeconds:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @failSafeExpiryLengthSeconds@
failSafeExpiryLengthSecondsSelector :: Selector
failSafeExpiryLengthSecondsSelector = mkSelector "failSafeExpiryLengthSeconds"

-- | @Selector@ for @setFailSafeExpiryLengthSeconds:@
setFailSafeExpiryLengthSecondsSelector :: Selector
setFailSafeExpiryLengthSecondsSelector = mkSelector "setFailSafeExpiryLengthSeconds:"

-- | @Selector@ for @maxCumulativeFailsafeSeconds@
maxCumulativeFailsafeSecondsSelector :: Selector
maxCumulativeFailsafeSecondsSelector = mkSelector "maxCumulativeFailsafeSeconds"

-- | @Selector@ for @setMaxCumulativeFailsafeSeconds:@
setMaxCumulativeFailsafeSecondsSelector :: Selector
setMaxCumulativeFailsafeSecondsSelector = mkSelector "setMaxCumulativeFailsafeSeconds:"

