{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDishwasherModeClusterModeTagStruct@.
module ObjC.Matter.MTRDishwasherModeClusterModeTagStruct
  ( MTRDishwasherModeClusterModeTagStruct
  , IsMTRDishwasherModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRDishwasherModeClusterModeTagStruct mtrDishwasherModeClusterModeTagStruct => mtrDishwasherModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrDishwasherModeClusterModeTagStruct  =
    sendMsg mtrDishwasherModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRDishwasherModeClusterModeTagStruct mtrDishwasherModeClusterModeTagStruct, IsNSNumber value) => mtrDishwasherModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrDishwasherModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRDishwasherModeClusterModeTagStruct mtrDishwasherModeClusterModeTagStruct => mtrDishwasherModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrDishwasherModeClusterModeTagStruct  =
    sendMsg mtrDishwasherModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRDishwasherModeClusterModeTagStruct mtrDishwasherModeClusterModeTagStruct, IsNSNumber value) => mtrDishwasherModeClusterModeTagStruct -> value -> IO ()
setValue mtrDishwasherModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

