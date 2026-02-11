{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalStateClusterOperationalErrorEvent@.
module ObjC.Matter.MTROperationalStateClusterOperationalErrorEvent
  ( MTROperationalStateClusterOperationalErrorEvent
  , IsMTROperationalStateClusterOperationalErrorEvent(..)
  , errorState
  , setErrorState
  , errorStateSelector
  , setErrorStateSelector


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

-- | @- errorState@
errorState :: IsMTROperationalStateClusterOperationalErrorEvent mtrOperationalStateClusterOperationalErrorEvent => mtrOperationalStateClusterOperationalErrorEvent -> IO (Id MTROperationalStateClusterErrorStateStruct)
errorState mtrOperationalStateClusterOperationalErrorEvent  =
    sendMsg mtrOperationalStateClusterOperationalErrorEvent (mkSelector "errorState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorState:@
setErrorState :: (IsMTROperationalStateClusterOperationalErrorEvent mtrOperationalStateClusterOperationalErrorEvent, IsMTROperationalStateClusterErrorStateStruct value) => mtrOperationalStateClusterOperationalErrorEvent -> value -> IO ()
setErrorState mtrOperationalStateClusterOperationalErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalStateClusterOperationalErrorEvent (mkSelector "setErrorState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorState@
errorStateSelector :: Selector
errorStateSelector = mkSelector "errorState"

-- | @Selector@ for @setErrorState:@
setErrorStateSelector :: Selector
setErrorStateSelector = mkSelector "setErrorState:"

