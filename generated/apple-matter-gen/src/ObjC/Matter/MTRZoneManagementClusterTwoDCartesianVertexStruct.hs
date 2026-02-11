{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterTwoDCartesianVertexStruct@.
module ObjC.Matter.MTRZoneManagementClusterTwoDCartesianVertexStruct
  ( MTRZoneManagementClusterTwoDCartesianVertexStruct
  , IsMTRZoneManagementClusterTwoDCartesianVertexStruct(..)
  , x
  , setX
  , y
  , setY
  , xSelector
  , setXSelector
  , ySelector
  , setYSelector


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

-- | @- x@
x :: IsMTRZoneManagementClusterTwoDCartesianVertexStruct mtrZoneManagementClusterTwoDCartesianVertexStruct => mtrZoneManagementClusterTwoDCartesianVertexStruct -> IO (Id NSNumber)
x mtrZoneManagementClusterTwoDCartesianVertexStruct  =
    sendMsg mtrZoneManagementClusterTwoDCartesianVertexStruct (mkSelector "x") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setX:@
setX :: (IsMTRZoneManagementClusterTwoDCartesianVertexStruct mtrZoneManagementClusterTwoDCartesianVertexStruct, IsNSNumber value) => mtrZoneManagementClusterTwoDCartesianVertexStruct -> value -> IO ()
setX mtrZoneManagementClusterTwoDCartesianVertexStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterTwoDCartesianVertexStruct (mkSelector "setX:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- y@
y :: IsMTRZoneManagementClusterTwoDCartesianVertexStruct mtrZoneManagementClusterTwoDCartesianVertexStruct => mtrZoneManagementClusterTwoDCartesianVertexStruct -> IO (Id NSNumber)
y mtrZoneManagementClusterTwoDCartesianVertexStruct  =
    sendMsg mtrZoneManagementClusterTwoDCartesianVertexStruct (mkSelector "y") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setY:@
setY :: (IsMTRZoneManagementClusterTwoDCartesianVertexStruct mtrZoneManagementClusterTwoDCartesianVertexStruct, IsNSNumber value) => mtrZoneManagementClusterTwoDCartesianVertexStruct -> value -> IO ()
setY mtrZoneManagementClusterTwoDCartesianVertexStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterTwoDCartesianVertexStruct (mkSelector "setY:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @x@
xSelector :: Selector
xSelector = mkSelector "x"

-- | @Selector@ for @setX:@
setXSelector :: Selector
setXSelector = mkSelector "setX:"

-- | @Selector@ for @y@
ySelector :: Selector
ySelector = mkSelector "y"

-- | @Selector@ for @setY:@
setYSelector :: Selector
setYSelector = mkSelector "setY:"

