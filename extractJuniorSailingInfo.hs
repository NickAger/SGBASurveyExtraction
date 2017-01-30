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

data SurveyFields = SurveyFields { contactDetails :: T.Text, age :: T.Text, membershipLength :: T.Text, dingyInstructor :: T.Text,  dingyInstructor2 :: T.Text, assistantInstructor :: T.Text , otherInstuctor :: T.Text , instructorText :: T.Text, juniorSailingInterest :: T.Text, involvedText :: T.Text }
  deriving (Generic, Show)

instance Csv.ToNamedRecord SurveyFields
instance Csv.DefaultOrdered SurveyFields

instance Csv.FromRecord SurveyFields where
    parseRecord v = SurveyFields <$> v .! 104 <*> v .! 16 <*> v .! 15 <*> v .! 27 <*> v .! 37 <*> v .! 36 <*> v .! 38 <*> v .! 39 <*> v .! 44 <*> v .! 103

readCSVLines filePath = do
  csvData <- BL.readFile filePath
  case Csv.decode Csv.HasHeader csvData  :: Either String (V.Vector SurveyFields) of
          Left err -> do
            putStrLn err
            return V.empty
          Right v -> return v

filterJuniorSailingInterest survey =
  let
    juniorSailingInterestText = juniorSailingInterest survey
  in
    juniorSailingInterestText /= "Little or no interest"  && (T.length juniorSailingInterestText /= 0)

main = do
  [filePath] <- Env.getArgs

  csvLines <- readCSVLines filePath
  let filteredLines = filter filterJuniorSailingInterest $ V.toList csvLines
  BL.putStr $ Csv.encodeDefaultOrderedByName filteredLines
