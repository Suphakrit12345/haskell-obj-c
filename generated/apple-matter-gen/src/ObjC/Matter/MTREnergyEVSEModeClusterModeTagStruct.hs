{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEModeClusterModeTagStruct@.
module ObjC.Matter.MTREnergyEVSEModeClusterModeTagStruct
  ( MTREnergyEVSEModeClusterModeTagStruct
  , IsMTREnergyEVSEModeClusterModeTagStruct(..)
  , mfgCode
  , setMfgCode
  , value
  , setValue
  , mfgCodeSelector
  , setMfgCodeSelector
  , valueSelector
  , setValueSelector


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

-- | @- mfgCode@
mfgCode :: IsMTREnergyEVSEModeClusterModeTagStruct mtrEnergyEVSEModeClusterModeTagStruct => mtrEnergyEVSEModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrEnergyEVSEModeClusterModeTagStruct  =
    sendMsg mtrEnergyEVSEModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTREnergyEVSEModeClusterModeTagStruct mtrEnergyEVSEModeClusterModeTagStruct, IsNSNumber value) => mtrEnergyEVSEModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrEnergyEVSEModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTREnergyEVSEModeClusterModeTagStruct mtrEnergyEVSEModeClusterModeTagStruct => mtrEnergyEVSEModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrEnergyEVSEModeClusterModeTagStruct  =
    sendMsg mtrEnergyEVSEModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTREnergyEVSEModeClusterModeTagStruct mtrEnergyEVSEModeClusterModeTagStruct, IsNSNumber value) => mtrEnergyEVSEModeClusterModeTagStruct -> value -> IO ()
setValue mtrEnergyEVSEModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mfgCode@
mfgCodeSelector :: Selector
mfgCodeSelector = mkSelector "mfgCode"

-- | @Selector@ for @setMfgCode:@
setMfgCodeSelector :: Selector
setMfgCodeSelector = mkSelector "setMfgCode:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

