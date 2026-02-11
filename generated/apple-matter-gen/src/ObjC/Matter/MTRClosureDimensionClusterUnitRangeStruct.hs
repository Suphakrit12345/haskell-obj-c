{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureDimensionClusterUnitRangeStruct@.
module ObjC.Matter.MTRClosureDimensionClusterUnitRangeStruct
  ( MTRClosureDimensionClusterUnitRangeStruct
  , IsMTRClosureDimensionClusterUnitRangeStruct(..)
  , min_
  , setMin
  , max_
  , setMax
  , minSelector
  , setMinSelector
  , maxSelector
  , setMaxSelector


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

-- | @- min@
min_ :: IsMTRClosureDimensionClusterUnitRangeStruct mtrClosureDimensionClusterUnitRangeStruct => mtrClosureDimensionClusterUnitRangeStruct -> IO (Id NSNumber)
min_ mtrClosureDimensionClusterUnitRangeStruct  =
    sendMsg mtrClosureDimensionClusterUnitRangeStruct (mkSelector "min") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMin:@
setMin :: (IsMTRClosureDimensionClusterUnitRangeStruct mtrClosureDimensionClusterUnitRangeStruct, IsNSNumber value) => mtrClosureDimensionClusterUnitRangeStruct -> value -> IO ()
setMin mtrClosureDimensionClusterUnitRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterUnitRangeStruct (mkSelector "setMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- max@
max_ :: IsMTRClosureDimensionClusterUnitRangeStruct mtrClosureDimensionClusterUnitRangeStruct => mtrClosureDimensionClusterUnitRangeStruct -> IO (Id NSNumber)
max_ mtrClosureDimensionClusterUnitRangeStruct  =
    sendMsg mtrClosureDimensionClusterUnitRangeStruct (mkSelector "max") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMax:@
setMax :: (IsMTRClosureDimensionClusterUnitRangeStruct mtrClosureDimensionClusterUnitRangeStruct, IsNSNumber value) => mtrClosureDimensionClusterUnitRangeStruct -> value -> IO ()
setMax mtrClosureDimensionClusterUnitRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterUnitRangeStruct (mkSelector "setMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @min@
minSelector :: Selector
minSelector = mkSelector "min"

-- | @Selector@ for @setMin:@
setMinSelector :: Selector
setMinSelector = mkSelector "setMin:"

-- | @Selector@ for @max@
maxSelector :: Selector
maxSelector = mkSelector "max"

-- | @Selector@ for @setMax:@
setMaxSelector :: Selector
setMaxSelector = mkSelector "setMax:"

