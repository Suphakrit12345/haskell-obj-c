{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterDimension@.
module ObjC.Matter.MTRContentLauncherClusterDimension
  ( MTRContentLauncherClusterDimension
  , IsMTRContentLauncherClusterDimension(..)
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
width :: IsMTRContentLauncherClusterDimension mtrContentLauncherClusterDimension => mtrContentLauncherClusterDimension -> IO (Id NSNumber)
width mtrContentLauncherClusterDimension  =
    sendMsg mtrContentLauncherClusterDimension (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsMTRContentLauncherClusterDimension mtrContentLauncherClusterDimension, IsNSNumber value) => mtrContentLauncherClusterDimension -> value -> IO ()
setWidth mtrContentLauncherClusterDimension  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterDimension (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- height@
height :: IsMTRContentLauncherClusterDimension mtrContentLauncherClusterDimension => mtrContentLauncherClusterDimension -> IO (Id NSNumber)
height mtrContentLauncherClusterDimension  =
    sendMsg mtrContentLauncherClusterDimension (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeight:@
setHeight :: (IsMTRContentLauncherClusterDimension mtrContentLauncherClusterDimension, IsNSNumber value) => mtrContentLauncherClusterDimension -> value -> IO ()
setHeight mtrContentLauncherClusterDimension  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterDimension (mkSelector "setHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metric@
metric :: IsMTRContentLauncherClusterDimension mtrContentLauncherClusterDimension => mtrContentLauncherClusterDimension -> IO (Id NSNumber)
metric mtrContentLauncherClusterDimension  =
    sendMsg mtrContentLauncherClusterDimension (mkSelector "metric") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetric:@
setMetric :: (IsMTRContentLauncherClusterDimension mtrContentLauncherClusterDimension, IsNSNumber value) => mtrContentLauncherClusterDimension -> value -> IO ()
setMetric mtrContentLauncherClusterDimension  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterDimension (mkSelector "setMetric:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

