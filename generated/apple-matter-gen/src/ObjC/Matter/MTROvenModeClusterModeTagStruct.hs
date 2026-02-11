{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenModeClusterModeTagStruct@.
module ObjC.Matter.MTROvenModeClusterModeTagStruct
  ( MTROvenModeClusterModeTagStruct
  , IsMTROvenModeClusterModeTagStruct(..)
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
mfgCode :: IsMTROvenModeClusterModeTagStruct mtrOvenModeClusterModeTagStruct => mtrOvenModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrOvenModeClusterModeTagStruct  =
    sendMsg mtrOvenModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTROvenModeClusterModeTagStruct mtrOvenModeClusterModeTagStruct, IsNSNumber value) => mtrOvenModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrOvenModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTROvenModeClusterModeTagStruct mtrOvenModeClusterModeTagStruct => mtrOvenModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrOvenModeClusterModeTagStruct  =
    sendMsg mtrOvenModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTROvenModeClusterModeTagStruct mtrOvenModeClusterModeTagStruct, IsNSNumber value) => mtrOvenModeClusterModeTagStruct -> value -> IO ()
setValue mtrOvenModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

