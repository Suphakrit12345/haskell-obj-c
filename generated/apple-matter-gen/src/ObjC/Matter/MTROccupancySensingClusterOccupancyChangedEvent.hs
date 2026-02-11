{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROccupancySensingClusterOccupancyChangedEvent@.
module ObjC.Matter.MTROccupancySensingClusterOccupancyChangedEvent
  ( MTROccupancySensingClusterOccupancyChangedEvent
  , IsMTROccupancySensingClusterOccupancyChangedEvent(..)
  , occupancy
  , setOccupancy
  , occupancySelector
  , setOccupancySelector


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

-- | @- occupancy@
occupancy :: IsMTROccupancySensingClusterOccupancyChangedEvent mtrOccupancySensingClusterOccupancyChangedEvent => mtrOccupancySensingClusterOccupancyChangedEvent -> IO (Id NSNumber)
occupancy mtrOccupancySensingClusterOccupancyChangedEvent  =
    sendMsg mtrOccupancySensingClusterOccupancyChangedEvent (mkSelector "occupancy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOccupancy:@
setOccupancy :: (IsMTROccupancySensingClusterOccupancyChangedEvent mtrOccupancySensingClusterOccupancyChangedEvent, IsNSNumber value) => mtrOccupancySensingClusterOccupancyChangedEvent -> value -> IO ()
setOccupancy mtrOccupancySensingClusterOccupancyChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOccupancySensingClusterOccupancyChangedEvent (mkSelector "setOccupancy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @occupancy@
occupancySelector :: Selector
occupancySelector = mkSelector "occupancy"

-- | @Selector@ for @setOccupancy:@
setOccupancySelector :: Selector
setOccupancySelector = mkSelector "setOccupancy:"

