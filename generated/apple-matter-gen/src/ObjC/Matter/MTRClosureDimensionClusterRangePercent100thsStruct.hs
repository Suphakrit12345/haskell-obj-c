{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureDimensionClusterRangePercent100thsStruct@.
module ObjC.Matter.MTRClosureDimensionClusterRangePercent100thsStruct
  ( MTRClosureDimensionClusterRangePercent100thsStruct
  , IsMTRClosureDimensionClusterRangePercent100thsStruct(..)
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
min_ :: IsMTRClosureDimensionClusterRangePercent100thsStruct mtrClosureDimensionClusterRangePercent100thsStruct => mtrClosureDimensionClusterRangePercent100thsStruct -> IO (Id NSNumber)
min_ mtrClosureDimensionClusterRangePercent100thsStruct  =
    sendMsg mtrClosureDimensionClusterRangePercent100thsStruct (mkSelector "min") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMin:@
setMin :: (IsMTRClosureDimensionClusterRangePercent100thsStruct mtrClosureDimensionClusterRangePercent100thsStruct, IsNSNumber value) => mtrClosureDimensionClusterRangePercent100thsStruct -> value -> IO ()
setMin mtrClosureDimensionClusterRangePercent100thsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterRangePercent100thsStruct (mkSelector "setMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- max@
max_ :: IsMTRClosureDimensionClusterRangePercent100thsStruct mtrClosureDimensionClusterRangePercent100thsStruct => mtrClosureDimensionClusterRangePercent100thsStruct -> IO (Id NSNumber)
max_ mtrClosureDimensionClusterRangePercent100thsStruct  =
    sendMsg mtrClosureDimensionClusterRangePercent100thsStruct (mkSelector "max") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMax:@
setMax :: (IsMTRClosureDimensionClusterRangePercent100thsStruct mtrClosureDimensionClusterRangePercent100thsStruct, IsNSNumber value) => mtrClosureDimensionClusterRangePercent100thsStruct -> value -> IO ()
setMax mtrClosureDimensionClusterRangePercent100thsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureDimensionClusterRangePercent100thsStruct (mkSelector "setMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

