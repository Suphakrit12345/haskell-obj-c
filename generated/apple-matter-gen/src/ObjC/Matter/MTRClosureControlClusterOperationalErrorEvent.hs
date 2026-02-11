{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterOperationalErrorEvent@.
module ObjC.Matter.MTRClosureControlClusterOperationalErrorEvent
  ( MTRClosureControlClusterOperationalErrorEvent
  , IsMTRClosureControlClusterOperationalErrorEvent(..)
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
errorState :: IsMTRClosureControlClusterOperationalErrorEvent mtrClosureControlClusterOperationalErrorEvent => mtrClosureControlClusterOperationalErrorEvent -> IO (Id NSArray)
errorState mtrClosureControlClusterOperationalErrorEvent  =
    sendMsg mtrClosureControlClusterOperationalErrorEvent (mkSelector "errorState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorState:@
setErrorState :: (IsMTRClosureControlClusterOperationalErrorEvent mtrClosureControlClusterOperationalErrorEvent, IsNSArray value) => mtrClosureControlClusterOperationalErrorEvent -> value -> IO ()
setErrorState mtrClosureControlClusterOperationalErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrClosureControlClusterOperationalErrorEvent (mkSelector "setErrorState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorState@
errorStateSelector :: Selector
errorStateSelector = mkSelector "errorState"

-- | @Selector@ for @setErrorState:@
setErrorStateSelector :: Selector
setErrorStateSelector = mkSelector "setErrorState:"

