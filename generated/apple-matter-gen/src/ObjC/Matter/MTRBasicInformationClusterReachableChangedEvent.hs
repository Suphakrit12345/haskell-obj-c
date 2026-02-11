{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicInformationClusterReachableChangedEvent@.
module ObjC.Matter.MTRBasicInformationClusterReachableChangedEvent
  ( MTRBasicInformationClusterReachableChangedEvent
  , IsMTRBasicInformationClusterReachableChangedEvent(..)
  , reachableNewValue
  , setReachableNewValue
  , reachableNewValueSelector
  , setReachableNewValueSelector


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

-- | @- reachableNewValue@
reachableNewValue :: IsMTRBasicInformationClusterReachableChangedEvent mtrBasicInformationClusterReachableChangedEvent => mtrBasicInformationClusterReachableChangedEvent -> IO (Id NSNumber)
reachableNewValue mtrBasicInformationClusterReachableChangedEvent  =
    sendMsg mtrBasicInformationClusterReachableChangedEvent (mkSelector "reachableNewValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReachableNewValue:@
setReachableNewValue :: (IsMTRBasicInformationClusterReachableChangedEvent mtrBasicInformationClusterReachableChangedEvent, IsNSNumber value) => mtrBasicInformationClusterReachableChangedEvent -> value -> IO ()
setReachableNewValue mtrBasicInformationClusterReachableChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBasicInformationClusterReachableChangedEvent (mkSelector "setReachableNewValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reachableNewValue@
reachableNewValueSelector :: Selector
reachableNewValueSelector = mkSelector "reachableNewValue"

-- | @Selector@ for @setReachableNewValue:@
setReachableNewValueSelector :: Selector
setReachableNewValueSelector = mkSelector "setReachableNewValue:"

