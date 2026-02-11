{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLaundryWasherModeClusterModeTagStruct@.
module ObjC.Matter.MTRLaundryWasherModeClusterModeTagStruct
  ( MTRLaundryWasherModeClusterModeTagStruct
  , IsMTRLaundryWasherModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRLaundryWasherModeClusterModeTagStruct mtrLaundryWasherModeClusterModeTagStruct => mtrLaundryWasherModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrLaundryWasherModeClusterModeTagStruct  =
    sendMsg mtrLaundryWasherModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRLaundryWasherModeClusterModeTagStruct mtrLaundryWasherModeClusterModeTagStruct, IsNSNumber value) => mtrLaundryWasherModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrLaundryWasherModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLaundryWasherModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRLaundryWasherModeClusterModeTagStruct mtrLaundryWasherModeClusterModeTagStruct => mtrLaundryWasherModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrLaundryWasherModeClusterModeTagStruct  =
    sendMsg mtrLaundryWasherModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRLaundryWasherModeClusterModeTagStruct mtrLaundryWasherModeClusterModeTagStruct, IsNSNumber value) => mtrLaundryWasherModeClusterModeTagStruct -> value -> IO ()
setValue mtrLaundryWasherModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLaundryWasherModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

