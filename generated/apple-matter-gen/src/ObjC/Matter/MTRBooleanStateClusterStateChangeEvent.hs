{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBooleanStateClusterStateChangeEvent@.
module ObjC.Matter.MTRBooleanStateClusterStateChangeEvent
  ( MTRBooleanStateClusterStateChangeEvent
  , IsMTRBooleanStateClusterStateChangeEvent(..)
  , stateValue
  , setStateValue
  , stateValueSelector
  , setStateValueSelector


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

-- | @- stateValue@
stateValue :: IsMTRBooleanStateClusterStateChangeEvent mtrBooleanStateClusterStateChangeEvent => mtrBooleanStateClusterStateChangeEvent -> IO (Id NSNumber)
stateValue mtrBooleanStateClusterStateChangeEvent  =
    sendMsg mtrBooleanStateClusterStateChangeEvent (mkSelector "stateValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStateValue:@
setStateValue :: (IsMTRBooleanStateClusterStateChangeEvent mtrBooleanStateClusterStateChangeEvent, IsNSNumber value) => mtrBooleanStateClusterStateChangeEvent -> value -> IO ()
setStateValue mtrBooleanStateClusterStateChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBooleanStateClusterStateChangeEvent (mkSelector "setStateValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stateValue@
stateValueSelector :: Selector
stateValueSelector = mkSelector "stateValue"

-- | @Selector@ for @setStateValue:@
setStateValueSelector :: Selector
setStateValueSelector = mkSelector "setStateValue:"

