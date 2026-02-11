{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterEngageStateChangedEvent@.
module ObjC.Matter.MTRClosureControlClusterEngageStateChangedEvent
  ( MTRClosureControlClusterEngageStateChangedEvent
  , IsMTRClosureControlClusterEngageStateChangedEvent(..)
  , engageValue
  , setEngageValue
  , engageValueSelector
  , setEngageValueSelector


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

-- | @- engageValue@
engageValue :: IsMTRClosureControlClusterEngageStateChangedEvent mtrClosureControlClusterEngageStateChangedEvent => mtrClosureControlClusterEngageStateChangedEvent -> IO (Id NSNumber)
engageValue mtrClosureControlClusterEngageStateChangedEvent  =
    sendMsg mtrClosureControlClusterEngageStateChangedEvent (mkSelector "engageValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEngageValue:@
setEngageValue :: (IsMTRClosureControlClusterEngageStateChangedEvent mtrClosureControlClusterEngageStateChangedEvent, IsNSNumber value) => mtrClosureControlClusterEngageStateChangedEvent -> value -> IO ()
setEngageValue mtrClosureControlClusterEngageStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterEngageStateChangedEvent (mkSelector "setEngageValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @engageValue@
engageValueSelector :: Selector
engageValueSelector = mkSelector "engageValue"

-- | @Selector@ for @setEngageValue:@
setEngageValueSelector :: Selector
setEngageValueSelector = mkSelector "setEngageValue:"

