{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BETextSuggestion@.
module ObjC.BrowserEngineKit.BETextSuggestion
  ( BETextSuggestion
  , IsBETextSuggestion(..)
  , initWithInputText
  , new
  , init_
  , inputText
  , initWithInputTextSelector
  , newSelector
  , initSelector
  , inputTextSelector


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

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a new text suggestion with the given input text.
--
-- ObjC selector: @- initWithInputText:@
initWithInputText :: (IsBETextSuggestion beTextSuggestion, IsNSString inputText) => beTextSuggestion -> inputText -> IO (Id BETextSuggestion)
initWithInputText beTextSuggestion  inputText =
  withObjCPtr inputText $ \raw_inputText ->
      sendMsg beTextSuggestion (mkSelector "initWithInputText:") (retPtr retVoid) [argPtr (castPtr raw_inputText :: Ptr ())] >>= ownedObject . castPtr

-- | @- new@
new :: IsBETextSuggestion beTextSuggestion => beTextSuggestion -> IO (Id BETextSuggestion)
new beTextSuggestion  =
    sendMsg beTextSuggestion (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsBETextSuggestion beTextSuggestion => beTextSuggestion -> IO (Id BETextSuggestion)
init_ beTextSuggestion  =
    sendMsg beTextSuggestion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Text that will be inserted into the document when the user chooses the suggestion.
--
-- ObjC selector: @- inputText@
inputText :: IsBETextSuggestion beTextSuggestion => beTextSuggestion -> IO (Id NSString)
inputText beTextSuggestion  =
    sendMsg beTextSuggestion (mkSelector "inputText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInputText:@
initWithInputTextSelector :: Selector
initWithInputTextSelector = mkSelector "initWithInputText:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @inputText@
inputTextSelector :: Selector
inputTextSelector = mkSelector "inputText"

