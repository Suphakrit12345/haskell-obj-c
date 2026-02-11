{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRModeSelectClusterSemanticTag@.
module ObjC.Matter.MTRModeSelectClusterSemanticTag
  ( MTRModeSelectClusterSemanticTag
  , IsMTRModeSelectClusterSemanticTag(..)
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
mfgCode :: IsMTRModeSelectClusterSemanticTag mtrModeSelectClusterSemanticTag => mtrModeSelectClusterSemanticTag -> IO (Id NSNumber)
mfgCode mtrModeSelectClusterSemanticTag  =
    sendMsg mtrModeSelectClusterSemanticTag (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRModeSelectClusterSemanticTag mtrModeSelectClusterSemanticTag, IsNSNumber value) => mtrModeSelectClusterSemanticTag -> value -> IO ()
setMfgCode mtrModeSelectClusterSemanticTag  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrModeSelectClusterSemanticTag (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRModeSelectClusterSemanticTag mtrModeSelectClusterSemanticTag => mtrModeSelectClusterSemanticTag -> IO (Id NSNumber)
value mtrModeSelectClusterSemanticTag  =
    sendMsg mtrModeSelectClusterSemanticTag (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRModeSelectClusterSemanticTag mtrModeSelectClusterSemanticTag, IsNSNumber value) => mtrModeSelectClusterSemanticTag -> value -> IO ()
setValue mtrModeSelectClusterSemanticTag  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrModeSelectClusterSemanticTag (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

