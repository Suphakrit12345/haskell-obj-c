{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKCDADocument@.
module ObjC.HealthKit.HKCDADocument
  ( HKCDADocument
  , IsHKCDADocument(..)
  , documentData
  , title
  , patientName
  , authorName
  , custodianName
  , documentDataSelector
  , titleSelector
  , patientNameSelector
  , authorNameSelector
  , custodianNameSelector


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

-- | documentData
--
-- The CDA document content in XML format as specified in the CDA standard. This may be nil if the            includeDocumentData option in HKDocumentQuery is specified as NO.
--
-- ObjC selector: @- documentData@
documentData :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSData)
documentData hkcdaDocument  =
    sendMsg hkcdaDocument (mkSelector "documentData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | title
--
-- The title of the document.
--
-- This property is extracted automatically from the document.
--
-- ObjC selector: @- title@
title :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSString)
title hkcdaDocument  =
    sendMsg hkcdaDocument (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | patientName
--
-- The name of the patient receiving treatment.
--
-- This property is extracted automatically from the document.
--
-- ObjC selector: @- patientName@
patientName :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSString)
patientName hkcdaDocument  =
    sendMsg hkcdaDocument (mkSelector "patientName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | authorName
--
-- The person responsible for authoring the document.  Usually, this is the treating physician.
--
-- This property is extracted automatically from the document.
--
-- ObjC selector: @- authorName@
authorName :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSString)
authorName hkcdaDocument  =
    sendMsg hkcdaDocument (mkSelector "authorName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | custodianName
--
-- The organization responsible for the document.  This is usually the treating institution name.
--
-- This property is extracted automatically from the document.
--
-- ObjC selector: @- custodianName@
custodianName :: IsHKCDADocument hkcdaDocument => hkcdaDocument -> IO (Id NSString)
custodianName hkcdaDocument  =
    sendMsg hkcdaDocument (mkSelector "custodianName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @documentData@
documentDataSelector :: Selector
documentDataSelector = mkSelector "documentData"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @patientName@
patientNameSelector :: Selector
patientNameSelector = mkSelector "patientName"

-- | @Selector@ for @authorName@
authorNameSelector :: Selector
authorNameSelector = mkSelector "authorName"

-- | @Selector@ for @custodianName@
custodianNameSelector :: Selector
custodianNameSelector = mkSelector "custodianName"

