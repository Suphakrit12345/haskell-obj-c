{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDate@.
module ObjC.SensorKit.NSDate
  ( NSDate
  , IsNSDate(..)
  , dateWithSRAbsoluteTime
  , initWithSRAbsoluteTime
  , srAbsoluteTime
  , dateWithSRAbsoluteTimeSelector
  , initWithSRAbsoluteTimeSelector
  , srAbsoluteTimeSelector


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

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ dateWithSRAbsoluteTime:@
dateWithSRAbsoluteTime :: CDouble -> IO (Id NSDate)
dateWithSRAbsoluteTime time =
  do
    cls' <- getRequiredClass "NSDate"
    sendClassMsg cls' (mkSelector "dateWithSRAbsoluteTime:") (retPtr retVoid) [argCDouble time] >>= retainedObject . castPtr

-- | @- initWithSRAbsoluteTime:@
initWithSRAbsoluteTime :: IsNSDate nsDate => nsDate -> CDouble -> IO (Id NSDate)
initWithSRAbsoluteTime nsDate  time =
    sendMsg nsDate (mkSelector "initWithSRAbsoluteTime:") (retPtr retVoid) [argCDouble time] >>= ownedObject . castPtr

-- | @- srAbsoluteTime@
srAbsoluteTime :: IsNSDate nsDate => nsDate -> IO CDouble
srAbsoluteTime nsDate  =
    sendMsg nsDate (mkSelector "srAbsoluteTime") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dateWithSRAbsoluteTime:@
dateWithSRAbsoluteTimeSelector :: Selector
dateWithSRAbsoluteTimeSelector = mkSelector "dateWithSRAbsoluteTime:"

-- | @Selector@ for @initWithSRAbsoluteTime:@
initWithSRAbsoluteTimeSelector :: Selector
initWithSRAbsoluteTimeSelector = mkSelector "initWithSRAbsoluteTime:"

-- | @Selector@ for @srAbsoluteTime@
srAbsoluteTimeSelector :: Selector
srAbsoluteTimeSelector = mkSelector "srAbsoluteTime"

