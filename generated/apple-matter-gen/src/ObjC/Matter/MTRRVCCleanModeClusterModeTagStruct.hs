{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCCleanModeClusterModeTagStruct@.
module ObjC.Matter.MTRRVCCleanModeClusterModeTagStruct
  ( MTRRVCCleanModeClusterModeTagStruct
  , IsMTRRVCCleanModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRRVCCleanModeClusterModeTagStruct mtrrvcCleanModeClusterModeTagStruct => mtrrvcCleanModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrrvcCleanModeClusterModeTagStruct  =
    sendMsg mtrrvcCleanModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRRVCCleanModeClusterModeTagStruct mtrrvcCleanModeClusterModeTagStruct, IsNSNumber value) => mtrrvcCleanModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrrvcCleanModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcCleanModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRRVCCleanModeClusterModeTagStruct mtrrvcCleanModeClusterModeTagStruct => mtrrvcCleanModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrrvcCleanModeClusterModeTagStruct  =
    sendMsg mtrrvcCleanModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRRVCCleanModeClusterModeTagStruct mtrrvcCleanModeClusterModeTagStruct, IsNSNumber value) => mtrrvcCleanModeClusterModeTagStruct -> value -> IO ()
setValue mtrrvcCleanModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcCleanModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

