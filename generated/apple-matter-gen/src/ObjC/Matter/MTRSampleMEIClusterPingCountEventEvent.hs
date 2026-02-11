{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSampleMEIClusterPingCountEventEvent@.
module ObjC.Matter.MTRSampleMEIClusterPingCountEventEvent
  ( MTRSampleMEIClusterPingCountEventEvent
  , IsMTRSampleMEIClusterPingCountEventEvent(..)
  , count
  , setCount
  , fabricIndex
  , setFabricIndex
  , countSelector
  , setCountSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- count@
count :: IsMTRSampleMEIClusterPingCountEventEvent mtrSampleMEIClusterPingCountEventEvent => mtrSampleMEIClusterPingCountEventEvent -> IO (Id NSNumber)
count mtrSampleMEIClusterPingCountEventEvent  =
    sendMsg mtrSampleMEIClusterPingCountEventEvent (mkSelector "count") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCount:@
setCount :: (IsMTRSampleMEIClusterPingCountEventEvent mtrSampleMEIClusterPingCountEventEvent, IsNSNumber value) => mtrSampleMEIClusterPingCountEventEvent -> value -> IO ()
setCount mtrSampleMEIClusterPingCountEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSampleMEIClusterPingCountEventEvent (mkSelector "setCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRSampleMEIClusterPingCountEventEvent mtrSampleMEIClusterPingCountEventEvent => mtrSampleMEIClusterPingCountEventEvent -> IO (Id NSNumber)
fabricIndex mtrSampleMEIClusterPingCountEventEvent  =
    sendMsg mtrSampleMEIClusterPingCountEventEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRSampleMEIClusterPingCountEventEvent mtrSampleMEIClusterPingCountEventEvent, IsNSNumber value) => mtrSampleMEIClusterPingCountEventEvent -> value -> IO ()
setFabricIndex mtrSampleMEIClusterPingCountEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSampleMEIClusterPingCountEventEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @setCount:@
setCountSelector :: Selector
setCountSelector = mkSelector "setCount:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

