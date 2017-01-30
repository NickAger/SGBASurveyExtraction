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

data SurveyFields = SurveyFields { contactDetails :: T.Text, age :: T.Text, membershipLength :: T.Text, pb1 :: T.Text,  pb2 :: T.Text, safetyBoat :: T.Text  }
  deriving (Generic, Show)

instance Csv.ToNamedRecord SurveyFields
instance Csv.DefaultOrdered SurveyFields

instance Csv.FromRecord SurveyFields where
    parseRecord v = SurveyFields <$> v .! 104 <*> v .! 16 <*> v .! 15 <*> v .! 28 <*> v .! 29 <*> v .! 30

readCSVLines filePath = do
  csvData <- BL.readFile filePath
  case Csv.decode Csv.NoHeader csvData  :: Either String (V.Vector SurveyFields) of
          Left err -> do
            putStrLn err
            return V.empty
          Right v -> return v

nonBlankPowerBoatLine survey =
  (T.length (pb1 survey) /= 0) || (T.length (pb2 survey) /= 0) || (T.length (safetyBoat survey) /= 0)

main = do
  [filePath] <- Env.getArgs

  csvLines <- readCSVLines filePath
  let filteredLines = filter nonBlankPowerBoatLine $ V.toList csvLines
  BL.putStr $ Csv.encodeDefaultOrderedByName filteredLines
