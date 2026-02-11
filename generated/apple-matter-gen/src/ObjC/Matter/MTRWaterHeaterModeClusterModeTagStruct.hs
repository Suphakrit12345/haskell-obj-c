{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterModeClusterModeTagStruct@.
module ObjC.Matter.MTRWaterHeaterModeClusterModeTagStruct
  ( MTRWaterHeaterModeClusterModeTagStruct
  , IsMTRWaterHeaterModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRWaterHeaterModeClusterModeTagStruct mtrWaterHeaterModeClusterModeTagStruct => mtrWaterHeaterModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrWaterHeaterModeClusterModeTagStruct  =
    sendMsg mtrWaterHeaterModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRWaterHeaterModeClusterModeTagStruct mtrWaterHeaterModeClusterModeTagStruct, IsNSNumber value) => mtrWaterHeaterModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrWaterHeaterModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRWaterHeaterModeClusterModeTagStruct mtrWaterHeaterModeClusterModeTagStruct => mtrWaterHeaterModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrWaterHeaterModeClusterModeTagStruct  =
    sendMsg mtrWaterHeaterModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRWaterHeaterModeClusterModeTagStruct mtrWaterHeaterModeClusterModeTagStruct, IsNSNumber value) => mtrWaterHeaterModeClusterModeTagStruct -> value -> IO ()
setValue mtrWaterHeaterModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

