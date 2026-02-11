{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAttributeValueWaiter@.
module ObjC.Matter.MTRAttributeValueWaiter
  ( MTRAttributeValueWaiter
  , IsMTRAttributeValueWaiter(..)
  , init_
  , new
  , cancel
  , uuid
  , initSelector
  , newSelector
  , cancelSelector
  , uuidSelector


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

-- | @- init@
init_ :: IsMTRAttributeValueWaiter mtrAttributeValueWaiter => mtrAttributeValueWaiter -> IO (Id MTRAttributeValueWaiter)
init_ mtrAttributeValueWaiter  =
    sendMsg mtrAttributeValueWaiter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRAttributeValueWaiter)
new  =
  do
    cls' <- getRequiredClass "MTRAttributeValueWaiter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Cancel the wait for the set of attribute path/value pairs represented by this MTRAttributeValueWaiter.  If the completion has not been called yet, it will becalled with MTRErrorCodeCancelled.
--
-- ObjC selector: @- cancel@
cancel :: IsMTRAttributeValueWaiter mtrAttributeValueWaiter => mtrAttributeValueWaiter -> IO ()
cancel mtrAttributeValueWaiter  =
    sendMsg mtrAttributeValueWaiter (mkSelector "cancel") retVoid []

-- | @- UUID@
uuid :: IsMTRAttributeValueWaiter mtrAttributeValueWaiter => mtrAttributeValueWaiter -> IO (Id NSUUID)
uuid mtrAttributeValueWaiter  =
    sendMsg mtrAttributeValueWaiter (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

