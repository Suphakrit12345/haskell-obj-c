{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterManagementClusterBoostStartedEvent@.
module ObjC.Matter.MTRWaterHeaterManagementClusterBoostStartedEvent
  ( MTRWaterHeaterManagementClusterBoostStartedEvent
  , IsMTRWaterHeaterManagementClusterBoostStartedEvent(..)
  , boostInfo
  , setBoostInfo
  , boostInfoSelector
  , setBoostInfoSelector


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

-- | @- boostInfo@
boostInfo :: IsMTRWaterHeaterManagementClusterBoostStartedEvent mtrWaterHeaterManagementClusterBoostStartedEvent => mtrWaterHeaterManagementClusterBoostStartedEvent -> IO (Id MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct)
boostInfo mtrWaterHeaterManagementClusterBoostStartedEvent  =
    sendMsg mtrWaterHeaterManagementClusterBoostStartedEvent (mkSelector "boostInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBoostInfo:@
setBoostInfo :: (IsMTRWaterHeaterManagementClusterBoostStartedEvent mtrWaterHeaterManagementClusterBoostStartedEvent, IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value) => mtrWaterHeaterManagementClusterBoostStartedEvent -> value -> IO ()
setBoostInfo mtrWaterHeaterManagementClusterBoostStartedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterManagementClusterBoostStartedEvent (mkSelector "setBoostInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boostInfo@
boostInfoSelector :: Selector
boostInfoSelector = mkSelector "boostInfo"

-- | @Selector@ for @setBoostInfo:@
setBoostInfoSelector :: Selector
setBoostInfoSelector = mkSelector "setBoostInfo:"

