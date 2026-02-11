{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKSampleQuery@.
module ObjC.HealthKit.HKSampleQuery
  ( HKSampleQuery
  , IsHKSampleQuery(..)
  , limit
  , sortDescriptors
  , limitSelector
  , sortDescriptorsSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | limit
--
-- The maximum number of results the receiver will return upon completion.
--
-- ObjC selector: @- limit@
limit :: IsHKSampleQuery hkSampleQuery => hkSampleQuery -> IO CULong
limit hkSampleQuery  =
    sendMsg hkSampleQuery (mkSelector "limit") retCULong []

-- | sortDescriptors
--
-- An array of NSSortDescriptors.
--
-- ObjC selector: @- sortDescriptors@
sortDescriptors :: IsHKSampleQuery hkSampleQuery => hkSampleQuery -> IO (Id NSArray)
sortDescriptors hkSampleQuery  =
    sendMsg hkSampleQuery (mkSelector "sortDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @limit@
limitSelector :: Selector
limitSelector = mkSelector "limit"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector
sortDescriptorsSelector = mkSelector "sortDescriptors"

