#!/usr/bin/env stack
-- stack runghc --resolver lts-7.15 --install-ghc --package text --package cassava

{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Text as T
import qualified System.Environment as Env
import qualified Data.Csv as Csv
import Data.Csv ((.!))
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import GHC.Generics

data SurveyFields = SurveyFields { contactDetails :: T.Text, age :: T.Text, membershipLength :: T.Text, assistantInstructor :: T.Text , otherInstuctor :: T.Text , instructorText :: T.Text }
  deriving (Generic, Show)

instance Csv.ToNamedRecord SurveyFields
instance Csv.DefaultOrdered SurveyFields

instance Csv.FromRecord SurveyFields where
    parseRecord v = SurveyFields <$> v .! 104 <*> v .! 16 <*> v .! 15 <*> v .! 36 <*> v .! 38 <*> v .! 39

readCSVLines filePath = do
  csvData <- BL.readFile filePath
  case Csv.decode Csv.HasHeader csvData  :: Either String (V.Vector SurveyFields) of
          Left err -> do
            putStrLn err
            return V.empty
          Right v -> return v

filterBlankSurveyLine :: SurveyFields -> Bool
filterBlankSurveyLine survey =
  (T.length (assistantInstructor survey) /= 0) || (T.length (otherInstuctor survey) /= 0) || (T.length (instructorText survey) /= 0)

main = do
  [filePath] <- Env.getArgs

  csvLines <- readCSVLines filePath
  let filteredLines = filter filterBlankSurveyLine $ V.toList csvLines
  BL.putStr $ Csv.encodeDefaultOrderedByName filteredLines
