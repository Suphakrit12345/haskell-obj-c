{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRModeSelectClusterSemanticTagStruct@.
module ObjC.Matter.MTRModeSelectClusterSemanticTagStruct
  ( MTRModeSelectClusterSemanticTagStruct
  , IsMTRModeSelectClusterSemanticTagStruct(..)
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
mfgCode :: IsMTRModeSelectClusterSemanticTagStruct mtrModeSelectClusterSemanticTagStruct => mtrModeSelectClusterSemanticTagStruct -> IO (Id NSNumber)
mfgCode mtrModeSelectClusterSemanticTagStruct  =
    sendMsg mtrModeSelectClusterSemanticTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRModeSelectClusterSemanticTagStruct mtrModeSelectClusterSemanticTagStruct, IsNSNumber value) => mtrModeSelectClusterSemanticTagStruct -> value -> IO ()
setMfgCode mtrModeSelectClusterSemanticTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrModeSelectClusterSemanticTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRModeSelectClusterSemanticTagStruct mtrModeSelectClusterSemanticTagStruct => mtrModeSelectClusterSemanticTagStruct -> IO (Id NSNumber)
value mtrModeSelectClusterSemanticTagStruct  =
    sendMsg mtrModeSelectClusterSemanticTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRModeSelectClusterSemanticTagStruct mtrModeSelectClusterSemanticTagStruct, IsNSNumber value) => mtrModeSelectClusterSemanticTagStruct -> value -> IO ()
setValue mtrModeSelectClusterSemanticTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrModeSelectClusterSemanticTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

