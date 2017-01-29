#!/usr/bin/env stack
-- stack runghc --resolver lts-7.15 --install-ghc --package text --package cassava

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Char
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.Environment
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import GHC.Generics

data SurveyFields = SurveyFields { assistantInstructor :: !T.Text , otherInstuctor :: !T.Text , instructorText :: !T.Text , contactDetails :: !T.Text }
  deriving (Generic, Show)

instance ToNamedRecord SurveyFields
instance DefaultOrdered SurveyFields

instance FromRecord SurveyFields where
    parseRecord v = SurveyFields <$> v .! 36 <*> v .! 38 <*> v .! 39 <*> v .! 104

extractFieldsFromLine fields =
    mconcat [assistantInstructor fields, " ,", otherInstuctor fields, " ,", instructorText fields, " ,", contactDetails fields]

readCSVLines filePath = do
  csvData <- BL.readFile filePath
  case decode NoHeader csvData  :: Either String (V.Vector SurveyFields) of
          Left err -> do
            putStrLn err
            return V.empty
          Right v -> return v

filterBlankSurveyLine :: SurveyFields -> Bool
filterBlankSurveyLine survey =
  (T.length (assistantInstructor survey) /= 0) || (T.length (otherInstuctor survey) /= 0) || (T.length (instructorText survey) /= 0)

encodeItemsToFile filePath =
  BL.writeFile filePath . encodeDefaultOrderedByName

main = do
  [filePath] <- getArgs
  progName <- getProgName

  csvLines <- readCSVLines filePath
  let filteredLines = filter filterBlankSurveyLine $ V.toList csvLines

  return (encodeItemsToFile "instructors.csv" filteredLines)
