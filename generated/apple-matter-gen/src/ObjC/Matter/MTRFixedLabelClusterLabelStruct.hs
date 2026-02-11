{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRFixedLabelClusterLabelStruct@.
module ObjC.Matter.MTRFixedLabelClusterLabelStruct
  ( MTRFixedLabelClusterLabelStruct
  , IsMTRFixedLabelClusterLabelStruct(..)
  , label
  , setLabel
  , value
  , setValue
  , labelSelector
  , setLabelSelector
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

-- | @- label@
label :: IsMTRFixedLabelClusterLabelStruct mtrFixedLabelClusterLabelStruct => mtrFixedLabelClusterLabelStruct -> IO (Id NSString)
label mtrFixedLabelClusterLabelStruct  =
    sendMsg mtrFixedLabelClusterLabelStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRFixedLabelClusterLabelStruct mtrFixedLabelClusterLabelStruct, IsNSString value) => mtrFixedLabelClusterLabelStruct -> value -> IO ()
setLabel mtrFixedLabelClusterLabelStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrFixedLabelClusterLabelStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRFixedLabelClusterLabelStruct mtrFixedLabelClusterLabelStruct => mtrFixedLabelClusterLabelStruct -> IO (Id NSString)
value mtrFixedLabelClusterLabelStruct  =
    sendMsg mtrFixedLabelClusterLabelStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRFixedLabelClusterLabelStruct mtrFixedLabelClusterLabelStruct, IsNSString value) => mtrFixedLabelClusterLabelStruct -> value -> IO ()
setValue mtrFixedLabelClusterLabelStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrFixedLabelClusterLabelStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

