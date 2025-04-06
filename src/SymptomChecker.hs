module SymptomChecker where

import Data.Text (Text, toLower)
import qualified Data.Text as T
import Data.List (sortBy)
import Data.Ord (comparing)

-- Helper function for case-insensitive string comparison
ciCompare :: Text -> Text -> Bool
ciCompare a b = toLower a == toLower b

-- Find matching conditions for given symptoms
findMatchingConditions :: [Text] -> [(Text, Int)]
findMatchingConditions symptoms = 
    let matches = [(condition, count) | (condition, conditionSymptoms) <- symptomDatabase,
                  let count = length [s | s <- symptoms, any (ciCompare s) conditionSymptoms],
                  count > 0]
    in sortBy (comparing (negate . snd)) matches

-- Get all available symptoms
allSymptoms :: [Text]
allSymptoms = concatMap snd symptomDatabase

-- Symptom database
symptomDatabase :: [(Text, [Text])]
symptomDatabase = 
    [ (T.pack "Common Cold", 
        [ T.pack "Runny nose"
        , T.pack "Sneezing"
        , T.pack "Cough"
        , T.pack "Sore throat"
        , T.pack "Congestion"
        , T.pack "Fatigue"
        ])
    , (T.pack "Flu", 
        [ T.pack "Fever"
        , T.pack "Chills"
        , T.pack "Body aches"
        , T.pack "Fatigue"
        , T.pack "Cough"
        , T.pack "Sore throat"
        , T.pack "Headache"
        ])
    , (T.pack "Migraine", 
        [ T.pack "Severe headache"
        , T.pack "Nausea"
        , T.pack "Sensitivity to light"
        , T.pack "Sensitivity to sound"
        , T.pack "Aura"
        ])
    , (T.pack "Strep Throat", 
        [ T.pack "Sore throat"
        , T.pack "Difficulty swallowing"
        , T.pack "Red tonsils"
        , T.pack "White patches on tonsils"
        , T.pack "Fever"
        ])
    , (T.pack "Bronchitis", 
        [ T.pack "Cough"
        , T.pack "Chest congestion"
        , T.pack "Shortness of breath"
        , T.pack "Fatigue"
        , T.pack "Wheezing"
        ])
    , (T.pack "Sinusitis", 
        [ T.pack "Facial pain"
        , T.pack "Nasal congestion"
        , T.pack "Runny nose"
        , T.pack "Headache"
        , T.pack "Cough"
        ])
    , (T.pack "Pneumonia", 
        [ T.pack "Cough"
        , T.pack "Fever"
        , T.pack "Shortness of breath"
        , T.pack "Chest pain"
        , T.pack "Fatigue"
        ])
    , (T.pack "Allergic Rhinitis", 
        [ T.pack "Runny nose"
        , T.pack "Sneezing"
        , T.pack "Itchy eyes"
        , T.pack "Nasal congestion"
        , T.pack "Watery eyes"
        ])
    , (T.pack "Gastroenteritis", 
        [ T.pack "Nausea"
        , T.pack "Vomiting"
        , T.pack "Diarrhea"
        , T.pack "Abdominal pain"
        , T.pack "Fever"
        ])
    , (T.pack "Anxiety", 
        [ T.pack "Rapid heartbeat"
        , T.pack "Sweating"
        , T.pack "Trembling"
        , T.pack "Shortness of breath"
        , T.pack "Restlessness"
        ])
    ] 