{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCRunModeClusterModeTagStruct@.
module ObjC.Matter.MTRRVCRunModeClusterModeTagStruct
  ( MTRRVCRunModeClusterModeTagStruct
  , IsMTRRVCRunModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRRVCRunModeClusterModeTagStruct mtrrvcRunModeClusterModeTagStruct => mtrrvcRunModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrrvcRunModeClusterModeTagStruct  =
    sendMsg mtrrvcRunModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRRVCRunModeClusterModeTagStruct mtrrvcRunModeClusterModeTagStruct, IsNSNumber value) => mtrrvcRunModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrrvcRunModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcRunModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRRVCRunModeClusterModeTagStruct mtrrvcRunModeClusterModeTagStruct => mtrrvcRunModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrrvcRunModeClusterModeTagStruct  =
    sendMsg mtrrvcRunModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRRVCRunModeClusterModeTagStruct mtrrvcRunModeClusterModeTagStruct, IsNSNumber value) => mtrrvcRunModeClusterModeTagStruct -> value -> IO ()
setValue mtrrvcRunModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcRunModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

