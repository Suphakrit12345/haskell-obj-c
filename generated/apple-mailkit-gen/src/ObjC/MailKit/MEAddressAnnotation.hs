{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An instance of this class can be used to change the visual style of recipeint email address token when user in composing a mail message.
--
-- Generated bindings for @MEAddressAnnotation@.
module ObjC.MailKit.MEAddressAnnotation
  ( MEAddressAnnotation
  , IsMEAddressAnnotation(..)
  , init_
  , new
  , errorWithLocalizedDescription
  , warningWithLocalizedDescription
  , successWithLocalizedDescription
  , initSelector
  , newSelector
  , errorWithLocalizedDescriptionSelector
  , warningWithLocalizedDescriptionSelector
  , successWithLocalizedDescriptionSelector


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

-- | @- init@
init_ :: IsMEAddressAnnotation meAddressAnnotation => meAddressAnnotation -> IO (Id MEAddressAnnotation)
init_ meAddressAnnotation  =
    sendMsg meAddressAnnotation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MEAddressAnnotation)
new  =
  do
    cls' <- getRequiredClass "MEAddressAnnotation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An annotation to denote a recipeint email address has an error when composing a mail message.
--
-- @localizedDescription@ — - A localized string with a brief description of the error that may be presented to the user.
--
-- ObjC selector: @+ errorWithLocalizedDescription:@
errorWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id MEAddressAnnotation)
errorWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "MEAddressAnnotation"
    withObjCPtr localizedDescription $ \raw_localizedDescription ->
      sendClassMsg cls' (mkSelector "errorWithLocalizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- | An annotation to warn about a recipeint email address when composing a mail message.
--
-- @localizedDescription@ — - A localized string with a brief description of the warning may be presented to the user. .
--
-- ObjC selector: @+ warningWithLocalizedDescription:@
warningWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id MEAddressAnnotation)
warningWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "MEAddressAnnotation"
    withObjCPtr localizedDescription $ \raw_localizedDescription ->
      sendClassMsg cls' (mkSelector "warningWithLocalizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- | An annotation to  denote a valid recipeint email address when composing a mail message.
--
-- @localizedDescription@ — - A localized string with a brief description that may be presented to the user. .
--
-- ObjC selector: @+ successWithLocalizedDescription:@
successWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id MEAddressAnnotation)
successWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "MEAddressAnnotation"
    withObjCPtr localizedDescription $ \raw_localizedDescription ->
      sendClassMsg cls' (mkSelector "successWithLocalizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @errorWithLocalizedDescription:@
errorWithLocalizedDescriptionSelector :: Selector
errorWithLocalizedDescriptionSelector = mkSelector "errorWithLocalizedDescription:"

-- | @Selector@ for @warningWithLocalizedDescription:@
warningWithLocalizedDescriptionSelector :: Selector
warningWithLocalizedDescriptionSelector = mkSelector "warningWithLocalizedDescription:"

-- | @Selector@ for @successWithLocalizedDescription:@
successWithLocalizedDescriptionSelector :: Selector
successWithLocalizedDescriptionSelector = mkSelector "successWithLocalizedDescription:"

