{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterSecureStateChangedEvent@.
module ObjC.Matter.MTRClosureControlClusterSecureStateChangedEvent
  ( MTRClosureControlClusterSecureStateChangedEvent
  , IsMTRClosureControlClusterSecureStateChangedEvent(..)
  , secureValue
  , setSecureValue
  , secureValueSelector
  , setSecureValueSelector


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

-- | @- secureValue@
secureValue :: IsMTRClosureControlClusterSecureStateChangedEvent mtrClosureControlClusterSecureStateChangedEvent => mtrClosureControlClusterSecureStateChangedEvent -> IO (Id NSNumber)
secureValue mtrClosureControlClusterSecureStateChangedEvent  =
    sendMsg mtrClosureControlClusterSecureStateChangedEvent (mkSelector "secureValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSecureValue:@
setSecureValue :: (IsMTRClosureControlClusterSecureStateChangedEvent mtrClosureControlClusterSecureStateChangedEvent, IsNSNumber value) => mtrClosureControlClusterSecureStateChangedEvent -> value -> IO ()
setSecureValue mtrClosureControlClusterSecureStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterSecureStateChangedEvent (mkSelector "setSecureValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @secureValue@
secureValueSelector :: Selector
secureValueSelector = mkSelector "secureValue"

-- | @Selector@ for @setSecureValue:@
setSecureValueSelector :: Selector
setSecureValueSelector = mkSelector "setSecureValue:"

