{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalGridConditionsClusterCurrentConditionsChangedEvent@.
module ObjC.Matter.MTRElectricalGridConditionsClusterCurrentConditionsChangedEvent
  ( MTRElectricalGridConditionsClusterCurrentConditionsChangedEvent
  , IsMTRElectricalGridConditionsClusterCurrentConditionsChangedEvent(..)
  , currentConditions
  , setCurrentConditions
  , currentConditionsSelector
  , setCurrentConditionsSelector


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

-- | @- currentConditions@
currentConditions :: IsMTRElectricalGridConditionsClusterCurrentConditionsChangedEvent mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent => mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent -> IO (Id MTRElectricalGridConditionsClusterElectricalGridConditionsStruct)
currentConditions mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent  =
    sendMsg mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent (mkSelector "currentConditions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentConditions:@
setCurrentConditions :: (IsMTRElectricalGridConditionsClusterCurrentConditionsChangedEvent mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent, IsMTRElectricalGridConditionsClusterElectricalGridConditionsStruct value) => mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent -> value -> IO ()
setCurrentConditions mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalGridConditionsClusterCurrentConditionsChangedEvent (mkSelector "setCurrentConditions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentConditions@
currentConditionsSelector :: Selector
currentConditionsSelector = mkSelector "currentConditions"

-- | @Selector@ for @setCurrentConditions:@
setCurrentConditionsSelector :: Selector
setCurrentConditionsSelector = mkSelector "setCurrentConditions:"

