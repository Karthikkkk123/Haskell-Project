module SymptomChecker where

import Data.Text (Text, toLower)
import qualified Data.Text as T
import Data.List (sortBy, find)
import Data.Ord (comparing)

-- Helper function for case-insensitive string comparison
ciCompare :: Text -> Text -> Bool
ciCompare a b = toLower a == toLower b

-- Age range type
data AgeRange = AgeRange
    { minAge :: Int
    , maxAge :: Int
    } deriving Show

-- Condition type with age range
data Condition = Condition
    { name :: Text
    , symptoms :: [Text]
    , ageRange :: Maybe AgeRange
    } deriving Show

-- Find matching conditions for given symptoms and age
findMatchingConditions :: [Text] -> Int -> [(Text, Int)]
findMatchingConditions inputSymptoms age = 
    let matches = [(name condition, count) | condition <- symptomDatabase,
                  let count = length [s | s <- inputSymptoms, any (ciCompare s) (symptoms condition)],
                  count > 0,
                  isAgeAppropriate condition age]
    in sortBy (comparing (negate . snd)) matches

-- Check if age is appropriate for the condition
isAgeAppropriate :: Condition -> Int -> Bool
isAgeAppropriate condition age = case ageRange condition of
    Nothing -> True
    Just (AgeRange min max) -> age >= min && age <= max

-- Get all available symptoms
allSymptoms :: [Text]
allSymptoms = concatMap symptoms symptomDatabase

-- Get age range for a condition
getConditionAgeRange :: Text -> Maybe (Int, Int)
getConditionAgeRange conditionName = 
    case find (\c -> name c == conditionName) symptomDatabase of
        Nothing -> Nothing
        Just condition -> case ageRange condition of
            Nothing -> Nothing
            Just (AgeRange min max) -> Just (min, max)

-- Symptom database with age ranges
symptomDatabase :: [Condition]
symptomDatabase = 
    [ Condition (T.pack "Common Cold") 
        [ T.pack "Runny nose"
        , T.pack "Sneezing"
        , T.pack "Cough"
        , T.pack "Sore throat"
        , T.pack "Congestion"
        , T.pack "Fatigue"
        ] Nothing
    , Condition (T.pack "Flu") 
        [ T.pack "Fever"
        , T.pack "Chills"
        , T.pack "Body aches"
        , T.pack "Fatigue"
        , T.pack "Cough"
        , T.pack "Sore throat"
        , T.pack "Headache"
        ] Nothing
    , Condition (T.pack "Migraine") 
        [ T.pack "Severe headache"
        , T.pack "Nausea"
        , T.pack "Sensitivity to light"
        , T.pack "Sensitivity to sound"
        , T.pack "Aura"
        ] (Just $ AgeRange 15 65)
    , Condition (T.pack "Strep Throat") 
        [ T.pack "Sore throat"
        , T.pack "Difficulty swallowing"
        , T.pack "Red tonsils"
        , T.pack "White patches on tonsils"
        , T.pack "Fever"
        ] (Just $ AgeRange 5 15)
    , Condition (T.pack "Bronchitis") 
        [ T.pack "Cough"
        , T.pack "Chest congestion"
        , T.pack "Shortness of breath"
        , T.pack "Fatigue"
        , T.pack "Wheezing"
        ] Nothing
    , Condition (T.pack "Sinusitis") 
        [ T.pack "Facial pain"
        , T.pack "Nasal congestion"
        , T.pack "Runny nose"
        , T.pack "Headache"
        , T.pack "Cough"
        ] Nothing
    , Condition (T.pack "Pneumonia") 
        [ T.pack "Cough"
        , T.pack "Fever"
        , T.pack "Shortness of breath"
        , T.pack "Chest pain"
        , T.pack "Fatigue"
        ] (Just $ AgeRange 0 100)
    , Condition (T.pack "Allergic Rhinitis") 
        [ T.pack "Runny nose"
        , T.pack "Sneezing"
        , T.pack "Itchy eyes"
        , T.pack "Nasal congestion"
        , T.pack "Watery eyes"
        ] Nothing
    , Condition (T.pack "Gastroenteritis") 
        [ T.pack "Nausea"
        , T.pack "Vomiting"
        , T.pack "Diarrhea"
        , T.pack "Abdominal pain"
        , T.pack "Fever"
        ] Nothing
    , Condition (T.pack "Anxiety") 
        [ T.pack "Rapid heartbeat"
        , T.pack "Sweating"
        , T.pack "Trembling"
        , T.pack "Shortness of breath"
        , T.pack "Restlessness"
        ] (Just $ AgeRange 13 100)
    ] 