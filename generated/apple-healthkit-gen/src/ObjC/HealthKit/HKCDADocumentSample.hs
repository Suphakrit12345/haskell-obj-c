{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKCDADocumentSample
--
-- A sample object representing a CDA document.
--
-- Generated bindings for @HKCDADocumentSample@.
module ObjC.HealthKit.HKCDADocumentSample
  ( HKCDADocumentSample
  , IsHKCDADocumentSample(..)
  , cdaDocumentSampleWithData_startDate_endDate_metadata_validationError
  , document
  , cdaDocumentSampleWithData_startDate_endDate_metadata_validationErrorSelector
  , documentSelector


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

-- | CDADocumentSampleWithData:startDate:endDate:device:metadata:validationError:
--
-- Creates a new document sample with the specified attributes.
--
-- @documentData@ — Document contents in an XML format that meets the CDA standard.
--
-- @startDate@ — The start date for the document.
--
-- @endDate@ — The end date for the document.
--
-- @metadata@ — Metadata for the document.
--
-- @validationError@ — The XML content will be validated against the standard for CDA content.  If that validation                        fails, then this parameter will be set with the relavant error.  Detailed information about the                        failure may be obtained by examining the value for the HKDetailedCDAValidationErrorKey key of                        the NSError's userInfo dictionary.
--
-- Returns: The new instance or nil if the documentData does not pass validation.
--
-- Attributes of the document, such as title, patient name, etc. will be extracted automatically                        from the document content.
--
-- ObjC selector: @+ CDADocumentSampleWithData:startDate:endDate:metadata:validationError:@
cdaDocumentSampleWithData_startDate_endDate_metadata_validationError :: (IsNSData documentData, IsNSDate startDate, IsNSDate endDate, IsNSDictionary metadata, IsNSError validationError) => documentData -> startDate -> endDate -> metadata -> validationError -> IO (Id HKCDADocumentSample)
cdaDocumentSampleWithData_startDate_endDate_metadata_validationError documentData startDate endDate metadata validationError =
  do
    cls' <- getRequiredClass "HKCDADocumentSample"
    withObjCPtr documentData $ \raw_documentData ->
      withObjCPtr startDate $ \raw_startDate ->
        withObjCPtr endDate $ \raw_endDate ->
          withObjCPtr metadata $ \raw_metadata ->
            withObjCPtr validationError $ \raw_validationError ->
              sendClassMsg cls' (mkSelector "CDADocumentSampleWithData:startDate:endDate:metadata:validationError:") (retPtr retVoid) [argPtr (castPtr raw_documentData :: Ptr ()), argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr raw_validationError :: Ptr ())] >>= retainedObject . castPtr

-- | document
--
-- The contents of the document.
--
-- Access to each CDA instance must be authorized by the user in order for the document data to be                accessible to an app.  The authorization request occurs the first time a document matches the predicate                of an executed HKDocumentQuery.  This property will always be nil if the sample is returned by an                HKSampleQuery or an HKAnchoredObjectQuery.
--
-- ObjC selector: @- document@
document :: IsHKCDADocumentSample hkcdaDocumentSample => hkcdaDocumentSample -> IO (Id HKCDADocument)
document hkcdaDocumentSample  =
    sendMsg hkcdaDocumentSample (mkSelector "document") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @CDADocumentSampleWithData:startDate:endDate:metadata:validationError:@
cdaDocumentSampleWithData_startDate_endDate_metadata_validationErrorSelector :: Selector
cdaDocumentSampleWithData_startDate_endDate_metadata_validationErrorSelector = mkSelector "CDADocumentSampleWithData:startDate:endDate:metadata:validationError:"

-- | @Selector@ for @document@
documentSelector :: Selector
documentSelector = mkSelector "document"

