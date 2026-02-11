{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapCamera@.
module ObjC.MapKit.MKMapCamera
  ( MKMapCamera
  , IsMKMapCamera(..)
  , camera
  , centerCoordinateDistance
  , setCenterCoordinateDistance
  , heading
  , setHeading
  , pitch
  , setPitch
  , altitude
  , setAltitude
  , cameraSelector
  , centerCoordinateDistanceSelector
  , setCenterCoordinateDistanceSelector
  , headingSelector
  , setHeadingSelector
  , pitchSelector
  , setPitchSelector
  , altitudeSelector
  , setAltitudeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ camera@
camera :: IO (Id MKMapCamera)
camera  =
  do
    cls' <- getRequiredClass "MKMapCamera"
    sendClassMsg cls' (mkSelector "camera") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- centerCoordinateDistance@
centerCoordinateDistance :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
centerCoordinateDistance mkMapCamera  =
    sendMsg mkMapCamera (mkSelector "centerCoordinateDistance") retCDouble []

-- | @- setCenterCoordinateDistance:@
setCenterCoordinateDistance :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setCenterCoordinateDistance mkMapCamera  value =
    sendMsg mkMapCamera (mkSelector "setCenterCoordinateDistance:") retVoid [argCDouble value]

-- | @- heading@
heading :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
heading mkMapCamera  =
    sendMsg mkMapCamera (mkSelector "heading") retCDouble []

-- | @- setHeading:@
setHeading :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setHeading mkMapCamera  value =
    sendMsg mkMapCamera (mkSelector "setHeading:") retVoid [argCDouble value]

-- | @- pitch@
pitch :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
pitch mkMapCamera  =
    sendMsg mkMapCamera (mkSelector "pitch") retCDouble []

-- | @- setPitch:@
setPitch :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setPitch mkMapCamera  value =
    sendMsg mkMapCamera (mkSelector "setPitch:") retVoid [argCDouble value]

-- | @- altitude@
altitude :: IsMKMapCamera mkMapCamera => mkMapCamera -> IO CDouble
altitude mkMapCamera  =
    sendMsg mkMapCamera (mkSelector "altitude") retCDouble []

-- | @- setAltitude:@
setAltitude :: IsMKMapCamera mkMapCamera => mkMapCamera -> CDouble -> IO ()
setAltitude mkMapCamera  value =
    sendMsg mkMapCamera (mkSelector "setAltitude:") retVoid [argCDouble value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @camera@
cameraSelector :: Selector
cameraSelector = mkSelector "camera"

-- | @Selector@ for @centerCoordinateDistance@
centerCoordinateDistanceSelector :: Selector
centerCoordinateDistanceSelector = mkSelector "centerCoordinateDistance"

-- | @Selector@ for @setCenterCoordinateDistance:@
setCenterCoordinateDistanceSelector :: Selector
setCenterCoordinateDistanceSelector = mkSelector "setCenterCoordinateDistance:"

-- | @Selector@ for @heading@
headingSelector :: Selector
headingSelector = mkSelector "heading"

-- | @Selector@ for @setHeading:@
setHeadingSelector :: Selector
setHeadingSelector = mkSelector "setHeading:"

-- | @Selector@ for @pitch@
pitchSelector :: Selector
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @setPitch:@
setPitchSelector :: Selector
setPitchSelector = mkSelector "setPitch:"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @setAltitude:@
setAltitudeSelector :: Selector
setAltitudeSelector = mkSelector "setAltitude:"

