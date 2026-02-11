{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains a flight number that the data detection system matches.
--
-- The DataDetection framework returns a flight number match in a @DDMatchFlightNumber@ object, which contains an airline name and flight number.
--
-- Generated bindings for @DDMatchFlightNumber@.
module ObjC.DataDetection.DDMatchFlightNumber
  ( DDMatchFlightNumber
  , IsDDMatchFlightNumber(..)
  , airline
  , flightNumber
  , airlineSelector
  , flightNumberSelector


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

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The name of an airline.
--
-- ObjC selector: @- airline@
airline :: IsDDMatchFlightNumber ddMatchFlightNumber => ddMatchFlightNumber -> IO (Id NSString)
airline ddMatchFlightNumber  =
    sendMsg ddMatchFlightNumber (mkSelector "airline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A string that represents a flight number.
--
-- ObjC selector: @- flightNumber@
flightNumber :: IsDDMatchFlightNumber ddMatchFlightNumber => ddMatchFlightNumber -> IO (Id NSString)
flightNumber ddMatchFlightNumber  =
    sendMsg ddMatchFlightNumber (mkSelector "flightNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @airline@
airlineSelector :: Selector
airlineSelector = mkSelector "airline"

-- | @Selector@ for @flightNumber@
flightNumberSelector :: Selector
flightNumberSelector = mkSelector "flightNumber"

