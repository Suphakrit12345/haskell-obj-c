{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMicrowaveOvenModeClusterModeTagStruct@.
module ObjC.Matter.MTRMicrowaveOvenModeClusterModeTagStruct
  ( MTRMicrowaveOvenModeClusterModeTagStruct
  , IsMTRMicrowaveOvenModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRMicrowaveOvenModeClusterModeTagStruct mtrMicrowaveOvenModeClusterModeTagStruct => mtrMicrowaveOvenModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrMicrowaveOvenModeClusterModeTagStruct  =
    sendMsg mtrMicrowaveOvenModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRMicrowaveOvenModeClusterModeTagStruct mtrMicrowaveOvenModeClusterModeTagStruct, IsNSNumber value) => mtrMicrowaveOvenModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrMicrowaveOvenModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRMicrowaveOvenModeClusterModeTagStruct mtrMicrowaveOvenModeClusterModeTagStruct => mtrMicrowaveOvenModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrMicrowaveOvenModeClusterModeTagStruct  =
    sendMsg mtrMicrowaveOvenModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRMicrowaveOvenModeClusterModeTagStruct mtrMicrowaveOvenModeClusterModeTagStruct, IsNSNumber value) => mtrMicrowaveOvenModeClusterModeTagStruct -> value -> IO ()
setValue mtrMicrowaveOvenModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

