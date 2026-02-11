{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterDimensionStruct@.
module ObjC.Matter.MTRContentLauncherClusterDimensionStruct
  ( MTRContentLauncherClusterDimensionStruct
  , IsMTRContentLauncherClusterDimensionStruct(..)
  , width
  , setWidth
  , height
  , setHeight
  , metric
  , setMetric
  , widthSelector
  , setWidthSelector
  , heightSelector
  , setHeightSelector
  , metricSelector
  , setMetricSelector


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

-- | @- width@
width :: IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct => mtrContentLauncherClusterDimensionStruct -> IO (Id NSNumber)
width mtrContentLauncherClusterDimensionStruct  =
    sendMsg mtrContentLauncherClusterDimensionStruct (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct, IsNSNumber value) => mtrContentLauncherClusterDimensionStruct -> value -> IO ()
setWidth mtrContentLauncherClusterDimensionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterDimensionStruct (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- height@
height :: IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct => mtrContentLauncherClusterDimensionStruct -> IO (Id NSNumber)
height mtrContentLauncherClusterDimensionStruct  =
    sendMsg mtrContentLauncherClusterDimensionStruct (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeight:@
setHeight :: (IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct, IsNSNumber value) => mtrContentLauncherClusterDimensionStruct -> value -> IO ()
setHeight mtrContentLauncherClusterDimensionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterDimensionStruct (mkSelector "setHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metric@
metric :: IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct => mtrContentLauncherClusterDimensionStruct -> IO (Id NSNumber)
metric mtrContentLauncherClusterDimensionStruct  =
    sendMsg mtrContentLauncherClusterDimensionStruct (mkSelector "metric") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetric:@
setMetric :: (IsMTRContentLauncherClusterDimensionStruct mtrContentLauncherClusterDimensionStruct, IsNSNumber value) => mtrContentLauncherClusterDimensionStruct -> value -> IO ()
setMetric mtrContentLauncherClusterDimensionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterDimensionStruct (mkSelector "setMetric:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @metric@
metricSelector :: Selector
metricSelector = mkSelector "metric"

-- | @Selector@ for @setMetric:@
setMetricSelector :: Selector
setMetricSelector = mkSelector "setMetric:"

