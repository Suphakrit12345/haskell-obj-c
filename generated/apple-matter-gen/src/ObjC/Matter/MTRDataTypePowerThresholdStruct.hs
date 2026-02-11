{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypePowerThresholdStruct@.
module ObjC.Matter.MTRDataTypePowerThresholdStruct
  ( MTRDataTypePowerThresholdStruct
  , IsMTRDataTypePowerThresholdStruct(..)
  , powerThreshold
  , setPowerThreshold
  , apparentPowerThreshold
  , setApparentPowerThreshold
  , powerThresholdSource
  , setPowerThresholdSource
  , powerThresholdSelector
  , setPowerThresholdSelector
  , apparentPowerThresholdSelector
  , setApparentPowerThresholdSelector
  , powerThresholdSourceSelector
  , setPowerThresholdSourceSelector


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

-- | @- powerThreshold@
powerThreshold :: IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct => mtrDataTypePowerThresholdStruct -> IO (Id NSNumber)
powerThreshold mtrDataTypePowerThresholdStruct  =
    sendMsg mtrDataTypePowerThresholdStruct (mkSelector "powerThreshold") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPowerThreshold:@
setPowerThreshold :: (IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct, IsNSNumber value) => mtrDataTypePowerThresholdStruct -> value -> IO ()
setPowerThreshold mtrDataTypePowerThresholdStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypePowerThresholdStruct (mkSelector "setPowerThreshold:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- apparentPowerThreshold@
apparentPowerThreshold :: IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct => mtrDataTypePowerThresholdStruct -> IO (Id NSNumber)
apparentPowerThreshold mtrDataTypePowerThresholdStruct  =
    sendMsg mtrDataTypePowerThresholdStruct (mkSelector "apparentPowerThreshold") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApparentPowerThreshold:@
setApparentPowerThreshold :: (IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct, IsNSNumber value) => mtrDataTypePowerThresholdStruct -> value -> IO ()
setApparentPowerThreshold mtrDataTypePowerThresholdStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypePowerThresholdStruct (mkSelector "setApparentPowerThreshold:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- powerThresholdSource@
powerThresholdSource :: IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct => mtrDataTypePowerThresholdStruct -> IO (Id NSNumber)
powerThresholdSource mtrDataTypePowerThresholdStruct  =
    sendMsg mtrDataTypePowerThresholdStruct (mkSelector "powerThresholdSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPowerThresholdSource:@
setPowerThresholdSource :: (IsMTRDataTypePowerThresholdStruct mtrDataTypePowerThresholdStruct, IsNSNumber value) => mtrDataTypePowerThresholdStruct -> value -> IO ()
setPowerThresholdSource mtrDataTypePowerThresholdStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypePowerThresholdStruct (mkSelector "setPowerThresholdSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerThreshold@
powerThresholdSelector :: Selector
powerThresholdSelector = mkSelector "powerThreshold"

-- | @Selector@ for @setPowerThreshold:@
setPowerThresholdSelector :: Selector
setPowerThresholdSelector = mkSelector "setPowerThreshold:"

-- | @Selector@ for @apparentPowerThreshold@
apparentPowerThresholdSelector :: Selector
apparentPowerThresholdSelector = mkSelector "apparentPowerThreshold"

-- | @Selector@ for @setApparentPowerThreshold:@
setApparentPowerThresholdSelector :: Selector
setApparentPowerThresholdSelector = mkSelector "setApparentPowerThreshold:"

-- | @Selector@ for @powerThresholdSource@
powerThresholdSourceSelector :: Selector
powerThresholdSourceSelector = mkSelector "powerThresholdSource"

-- | @Selector@ for @setPowerThresholdSource:@
setPowerThresholdSourceSelector :: Selector
setPowerThresholdSourceSelector = mkSelector "setPowerThresholdSource:"

