{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRElectrocardiogramData@.
module ObjC.SensorKit.SRElectrocardiogramData
  ( SRElectrocardiogramData
  , IsSRElectrocardiogramData(..)
  , init_
  , new
  , flags
  , value
  , initSelector
  , newSelector
  , flagsSelector
  , valueSelector

  -- * Enum types
  , SRElectrocardiogramDataFlags(SRElectrocardiogramDataFlags)
  , pattern SRElectrocardiogramDataFlagsNone
  , pattern SRElectrocardiogramDataFlagsSignalInvalid
  , pattern SRElectrocardiogramDataFlagsCrownTouched

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
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRElectrocardiogramData srElectrocardiogramData => srElectrocardiogramData -> IO (Id SRElectrocardiogramData)
init_ srElectrocardiogramData  =
    sendMsg srElectrocardiogramData (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRElectrocardiogramData)
new  =
  do
    cls' <- getRequiredClass "SRElectrocardiogramData"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- flags@
flags :: IsSRElectrocardiogramData srElectrocardiogramData => srElectrocardiogramData -> IO SRElectrocardiogramDataFlags
flags srElectrocardiogramData  =
    fmap (coerce :: CULong -> SRElectrocardiogramDataFlags) $ sendMsg srElectrocardiogramData (mkSelector "flags") retCULong []

-- | value
--
-- Value of the ECG AC data in microvolts
--
-- ObjC selector: @- value@
value :: IsSRElectrocardiogramData srElectrocardiogramData => srElectrocardiogramData -> IO (Id NSMeasurement)
value srElectrocardiogramData  =
    sendMsg srElectrocardiogramData (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @flags@
flagsSelector :: Selector
flagsSelector = mkSelector "flags"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

