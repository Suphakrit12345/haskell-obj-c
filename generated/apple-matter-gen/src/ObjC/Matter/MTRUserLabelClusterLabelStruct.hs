{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUserLabelClusterLabelStruct@.
module ObjC.Matter.MTRUserLabelClusterLabelStruct
  ( MTRUserLabelClusterLabelStruct
  , IsMTRUserLabelClusterLabelStruct(..)
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
label :: IsMTRUserLabelClusterLabelStruct mtrUserLabelClusterLabelStruct => mtrUserLabelClusterLabelStruct -> IO (Id NSString)
label mtrUserLabelClusterLabelStruct  =
    sendMsg mtrUserLabelClusterLabelStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRUserLabelClusterLabelStruct mtrUserLabelClusterLabelStruct, IsNSString value) => mtrUserLabelClusterLabelStruct -> value -> IO ()
setLabel mtrUserLabelClusterLabelStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUserLabelClusterLabelStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRUserLabelClusterLabelStruct mtrUserLabelClusterLabelStruct => mtrUserLabelClusterLabelStruct -> IO (Id NSString)
value mtrUserLabelClusterLabelStruct  =
    sendMsg mtrUserLabelClusterLabelStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRUserLabelClusterLabelStruct mtrUserLabelClusterLabelStruct, IsNSString value) => mtrUserLabelClusterLabelStruct -> value -> IO ()
setValue mtrUserLabelClusterLabelStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUserLabelClusterLabelStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

