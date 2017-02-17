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

data SurveyFields = SurveyFields { contactDetailsImport :: T.Text, dinghy1 :: T.Text, dinghy2 :: T.Text, dinghy3 :: T.Text, dinghyInstructor :: T.Text, pb1 :: T.Text,  pb2 :: T.Text, safetyBoat :: T.Text, canoe :: T.Text, assistantInstructor :: T.Text, dinghyInstructor2 :: T.Text, qualificationComment :: T.Text  }
  deriving (Generic, Show)

instance Csv.ToNamedRecord SurveyFields
instance Csv.DefaultOrdered SurveyFields


data ExportFields = ExportFields { surveyNumber :: Int, contactDetails :: T.Text, wouldLikeToGet:: T.Text, comments :: T.Text } deriving (Generic, Show)
instance Csv.ToNamedRecord ExportFields
instance Csv.DefaultOrdered ExportFields

instance Csv.FromRecord SurveyFields where
    parseRecord v = SurveyFields <$> v .! 104 <*> v .! 24 <*> v .! 25 <*> v .! 26 <*> v .! 27 <*> v .! 28 <*> v .! 29 <*> v .! 30 <*> v .! 35 <*> v .! 36 <*> v .! 37 <*> v .! 39

readCSVLines filePath = do
  csvData <- BL.readFile filePath
  case Csv.decode Csv.HasHeader csvData  :: Either String (V.Vector SurveyFields) of
          Left err -> do
            putStrLn err
            return V.empty
          Right v -> return v

filterBlankSurveyLine :: (SurveyFields, Int) -> Bool
filterBlankSurveyLine (survey, _) =
  (dinghy1 survey == "Would like to get") || (dinghy2 survey == "Would like to get") || (dinghy3 survey == "Would like to get") || (dinghyInstructor survey == "Would like to get") || (pb1 survey == "Would like to get") || (pb2 survey == "Would like to get") || (safetyBoat survey == "Would like to get") || (canoe survey == "Would like to get") || (assistantInstructor survey == "Would like to get") || (dinghyInstructor2 survey == "Would like to get") || (T.length (qualificationComment survey) /= 0)

exportField fieldText fieldName = 
  if fieldText == "Would like to get" then Just fieldName else Nothing

exportDinghyInstructor survey = 
  if (dinghyInstructor survey == "Would like to get") || (dinghyInstructor2 survey == "Would like to get") then Just "Dinghy Instructor" else Nothing

exportPB2 survey = 
  if (pb1 survey == "Would like to get") || (pb2 survey == "Would like to get") then Just "PB2" else Nothing
  

mappingSurveyToExport :: (SurveyFields, Int) -> ExportFields
mappingSurveyToExport (survey, i) = ExportFields i (if T.length (contactDetailsImport survey) == 0 then "NO CONTACT DETAILS" else contactDetailsImport survey)  
  (T.intercalate ", " $  catMaybes [exportField (dinghy1 survey) "RYA Dinghy level 1", 
    exportField (dinghy2 survey) "RYA Dinghy level 2",
    exportField (dinghy3 survey) "RYA Dinghy level 3",
    exportDinghyInstructor survey,
    exportPB2 survey,
    exportField (safetyBoat survey) "RYA Safety Boat",
    exportField (canoe survey) "canoeing qualification",
    exportField (assistantInstructor survey) "Assistant Instructor"]) $
    qualificationComment survey

main = do
  [filePath] <- Env.getArgs

  csvLines <- readCSVLines filePath
  let linesWithSurveyIndex = map (\(t, i) -> (t, 138-i)) $ zip (V.toList csvLines) [0..]
  let filteredLines = filter filterBlankSurveyLine linesWithSurveyIndex
  let descriptionLines = map mappingSurveyToExport filteredLines

  BL.putStr $ Csv.encodeDefaultOrderedByName descriptionLines