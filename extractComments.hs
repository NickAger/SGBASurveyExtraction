#!/usr/bin/env stack
-- stack runghc --resolver lts-7.15 --install-ghc --package text --package cassava

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified System.Environment as Env
import qualified Data.Csv as Csv
import Data.Csv ((.!))
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)

data SurveyFields = SurveyFields { contactDetailsImport :: T.Text,  experienceOnWaterComment :: T.Text,  instructorText :: T.Text, howdoYouUseTheClub :: T.Text, craftCommentsUseOwn :: T.Text, moreUseOfTheClub :: T.Text, clubFacilities :: T.Text, moreAttractive :: T.Text, otherHelpToInvolvementComment :: T.Text, otherEvents :: T.Text, otherComments :: T.Text }
  deriving (Generic, Show)
instance Csv.ToNamedRecord SurveyFields
instance Csv.DefaultOrdered SurveyFields

data ExportFields = ExportFields { surveyNumber :: Int, contactDetails :: T.Text, comments :: T.Text } deriving (Generic, Show)
instance Csv.ToNamedRecord ExportFields
instance Csv.DefaultOrdered ExportFields

instance Csv.FromRecord SurveyFields where
    parseRecord v = SurveyFields <$> v .! 104 <*> v .! 23 <*>  v .! 39 <*>  v .! 53 <*> v .! 67 <*> v .! 81 <*> v .! 91 <*> v .! 92 <*> v .! 102 <*> v .! 103 <*> v .! 105

readCSVLines filePath = do
  csvData <- BL.readFile filePath
  case Csv.decode Csv.HasHeader csvData  :: Either String (V.Vector SurveyFields) of
          Left err -> do
            putStrLn err
            return V.empty
          Right v -> return v

filterBlankSurveyLine :: (SurveyFields, Int) -> Bool
filterBlankSurveyLine (survey, _) =
  (T.length (experienceOnWaterComment survey) /= 0) || (T.length (instructorText survey) /= 0) || (T.length (howdoYouUseTheClub survey) /= 0) || (T.length (craftCommentsUseOwn survey) /= 0) || (T.length (moreUseOfTheClub survey) /= 0) || (T.length (clubFacilities survey) /= 0) || (T.length (moreAttractive survey) /= 0) || (T.length (otherHelpToInvolvementComment survey) /= 0) || (T.length (otherEvents survey) /= 0) || (T.length (otherComments survey) /= 0)

exportField fieldText fieldName = 
  if T.length fieldText == 0 then Nothing else Just $ "'" <> fieldName <> ": " <> fieldText <> "'"

mappingSurveyToExport :: (SurveyFields, Int) -> ExportFields
mappingSurveyToExport (survey, i) = ExportFields i (if T.length (contactDetailsImport survey) == 0 then "NO CONTACT DETAILS" else contactDetailsImport survey) $ 
  T.intercalate "\n" $ catMaybes [exportField (experienceOnWaterComment survey) "experienceOnWaterComment", 
    exportField (instructorText survey) "instructorText",
    exportField (howdoYouUseTheClub survey) "howdoYouUseTheClub",
    exportField (craftCommentsUseOwn survey) "craftCommentsUseOwn",
    exportField (moreUseOfTheClub survey) "moreUseOfTheClub",
    exportField (clubFacilities survey) "clubFacilities",
    exportField (moreAttractive survey) "moreAttractive",
    exportField (otherHelpToInvolvementComment survey) "otherHelpToInvolvementComment",
    exportField (otherEvents survey) "otherEvents",
    exportField (otherComments survey) "otherComments"]

main = do
  [filePath] <- Env.getArgs

  csvLines <- readCSVLines filePath
  let linesWithSurveyIndex = map (\(t, i) -> (t, 138-i)) $ zip (V.toList csvLines) [0..]
  let filteredLines = filter filterBlankSurveyLine linesWithSurveyIndex
  let descriptionLines = map mappingSurveyToExport filteredLines

  BL.putStr $ Csv.encodeDefaultOrderedByName descriptionLines
