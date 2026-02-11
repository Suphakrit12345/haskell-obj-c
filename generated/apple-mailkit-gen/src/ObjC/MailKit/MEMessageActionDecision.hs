{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MEMessageActionDecision@.
module ObjC.MailKit.MEMessageActionDecision
  ( MEMessageActionDecision
  , IsMEMessageActionDecision(..)
  , decisionApplyingAction
  , decisionApplyingActions
  , new
  , init_
  , invokeAgainWithBody
  , decisionApplyingActionSelector
  , decisionApplyingActionsSelector
  , newSelector
  , initSelector
  , invokeAgainWithBodySelector


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

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ decisionApplyingAction:@
decisionApplyingAction :: IsMEMessageAction action => action -> IO (Id MEMessageActionDecision)
decisionApplyingAction action =
  do
    cls' <- getRequiredClass "MEMessageActionDecision"
    withObjCPtr action $ \raw_action ->
      sendClassMsg cls' (mkSelector "decisionApplyingAction:") (retPtr retVoid) [argPtr (castPtr raw_action :: Ptr ())] >>= retainedObject . castPtr

-- | Creates an @MEMessageActionDecision@ with multiple actions. Conflicting actions will be ignored.
--
-- ObjC selector: @+ decisionApplyingActions:@
decisionApplyingActions :: IsNSArray actions => actions -> IO (Id MEMessageActionDecision)
decisionApplyingActions actions =
  do
    cls' <- getRequiredClass "MEMessageActionDecision"
    withObjCPtr actions $ \raw_actions ->
      sendClassMsg cls' (mkSelector "decisionApplyingActions:") (retPtr retVoid) [argPtr (castPtr raw_actions :: Ptr ())] >>= retainedObject . castPtr

-- | @+ new@
new :: IO (Id MEMessageActionDecision)
new  =
  do
    cls' <- getRequiredClass "MEMessageActionDecision"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEMessageActionDecision meMessageActionDecision => meMessageActionDecision -> IO (Id MEMessageActionDecision)
init_ meMessageActionDecision  =
    sendMsg meMessageActionDecision (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ invokeAgainWithBody@
invokeAgainWithBody :: IO (Id MEMessageActionDecision)
invokeAgainWithBody  =
  do
    cls' <- getRequiredClass "MEMessageActionDecision"
    sendClassMsg cls' (mkSelector "invokeAgainWithBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @decisionApplyingAction:@
decisionApplyingActionSelector :: Selector
decisionApplyingActionSelector = mkSelector "decisionApplyingAction:"

-- | @Selector@ for @decisionApplyingActions:@
decisionApplyingActionsSelector :: Selector
decisionApplyingActionsSelector = mkSelector "decisionApplyingActions:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @invokeAgainWithBody@
invokeAgainWithBodySelector :: Selector
invokeAgainWithBodySelector = mkSelector "invokeAgainWithBody"

