{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROccupancySensingClusterHoldTimeLimitsStruct@.
module ObjC.Matter.MTROccupancySensingClusterHoldTimeLimitsStruct
  ( MTROccupancySensingClusterHoldTimeLimitsStruct
  , IsMTROccupancySensingClusterHoldTimeLimitsStruct(..)
  , holdTimeMin
  , setHoldTimeMin
  , holdTimeMax
  , setHoldTimeMax
  , holdTimeDefault
  , setHoldTimeDefault
  , holdTimeMinSelector
  , setHoldTimeMinSelector
  , holdTimeMaxSelector
  , setHoldTimeMaxSelector
  , holdTimeDefaultSelector
  , setHoldTimeDefaultSelector


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

-- | @- holdTimeMin@
holdTimeMin :: IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct => mtrOccupancySensingClusterHoldTimeLimitsStruct -> IO (Id NSNumber)
holdTimeMin mtrOccupancySensingClusterHoldTimeLimitsStruct  =
    sendMsg mtrOccupancySensingClusterHoldTimeLimitsStruct (mkSelector "holdTimeMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHoldTimeMin:@
setHoldTimeMin :: (IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct, IsNSNumber value) => mtrOccupancySensingClusterHoldTimeLimitsStruct -> value -> IO ()
setHoldTimeMin mtrOccupancySensingClusterHoldTimeLimitsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOccupancySensingClusterHoldTimeLimitsStruct (mkSelector "setHoldTimeMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- holdTimeMax@
holdTimeMax :: IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct => mtrOccupancySensingClusterHoldTimeLimitsStruct -> IO (Id NSNumber)
holdTimeMax mtrOccupancySensingClusterHoldTimeLimitsStruct  =
    sendMsg mtrOccupancySensingClusterHoldTimeLimitsStruct (mkSelector "holdTimeMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHoldTimeMax:@
setHoldTimeMax :: (IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct, IsNSNumber value) => mtrOccupancySensingClusterHoldTimeLimitsStruct -> value -> IO ()
setHoldTimeMax mtrOccupancySensingClusterHoldTimeLimitsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOccupancySensingClusterHoldTimeLimitsStruct (mkSelector "setHoldTimeMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- holdTimeDefault@
holdTimeDefault :: IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct => mtrOccupancySensingClusterHoldTimeLimitsStruct -> IO (Id NSNumber)
holdTimeDefault mtrOccupancySensingClusterHoldTimeLimitsStruct  =
    sendMsg mtrOccupancySensingClusterHoldTimeLimitsStruct (mkSelector "holdTimeDefault") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHoldTimeDefault:@
setHoldTimeDefault :: (IsMTROccupancySensingClusterHoldTimeLimitsStruct mtrOccupancySensingClusterHoldTimeLimitsStruct, IsNSNumber value) => mtrOccupancySensingClusterHoldTimeLimitsStruct -> value -> IO ()
setHoldTimeDefault mtrOccupancySensingClusterHoldTimeLimitsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOccupancySensingClusterHoldTimeLimitsStruct (mkSelector "setHoldTimeDefault:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @holdTimeMin@
holdTimeMinSelector :: Selector
holdTimeMinSelector = mkSelector "holdTimeMin"

-- | @Selector@ for @setHoldTimeMin:@
setHoldTimeMinSelector :: Selector
setHoldTimeMinSelector = mkSelector "setHoldTimeMin:"

-- | @Selector@ for @holdTimeMax@
holdTimeMaxSelector :: Selector
holdTimeMaxSelector = mkSelector "holdTimeMax"

-- | @Selector@ for @setHoldTimeMax:@
setHoldTimeMaxSelector :: Selector
setHoldTimeMaxSelector = mkSelector "setHoldTimeMax:"

-- | @Selector@ for @holdTimeDefault@
holdTimeDefaultSelector :: Selector
holdTimeDefaultSelector = mkSelector "holdTimeDefault"

-- | @Selector@ for @setHoldTimeDefault:@
setHoldTimeDefaultSelector :: Selector
setHoldTimeDefaultSelector = mkSelector "setHoldTimeDefault:"

