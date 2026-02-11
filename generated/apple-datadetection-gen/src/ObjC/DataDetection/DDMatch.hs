{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A base class for common types of data that the data detection system matches.
--
-- The DataDetection framework returns results in objects that are subclasses of @DDMatch@, which are specific to the type of matching data. Each object contains the matched string.
--
-- Generated bindings for @DDMatch@.
module ObjC.DataDetection.DDMatch
  ( DDMatch
  , IsDDMatch(..)
  , init_
  , matchedString
  , initSelector
  , matchedStringSelector


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

-- | @- init@
init_ :: IsDDMatch ddMatch => ddMatch -> IO (Id DDMatch)
init_ ddMatch  =
    sendMsg ddMatch (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A substring that the data detection system identifies from an original string as a common type of data.
--
-- Use @DDMatch@ subclasses that the data detection system provides for a semantic interpretation of this string.
--
-- ObjC selector: @- matchedString@
matchedString :: IsDDMatch ddMatch => ddMatch -> IO (Id NSString)
matchedString ddMatch  =
    sendMsg ddMatch (mkSelector "matchedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @matchedString@
matchedStringSelector :: Selector
matchedStringSelector = mkSelector "matchedString"

