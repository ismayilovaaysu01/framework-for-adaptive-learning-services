{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes #-}

module Rodriguez where

import Recommendation

import Data.List (maximumBy)
import Data.Ord (comparing)
-- This file provides a conceptual Haskell representation of the core system
-- described in the paper by Rodriguez-Martinez et al. (2023).
-- The paper focuses on building a personalized homework system for fifth-grade
-- students' understanding of fractions using a learning analytics-based
-- formative assessment.

-- | ---------------------------
-- | Data Model Implementation
-- | ---------------------------

-- Represents the different concepts related to fractions that students learn.

data FractionConcept  =  AddingFractions
                     | SubtractingFractions
                     | SimplifyingFractions
                     | ConvertingFractions
                     | OtherFractionConcept
                     deriving (Show, Eq)

-- Represents a single homework problem.
data FractionProblem  =  Problem { problemId :: Int
                                 , problemConcept :: FractionConcept
                                 } deriving (Show, Eq)

-- A placeholder for the Learning Analytics (LA) Model.
-- This model would store the results of the formative assessments and the
-- student's performance data.
data LAAssessmentModel  =  LAAssessmentModel { assessmentData  :: String -- Placeholder for assessment data
                                             , problemPool     :: [FractionProblem] -- The set of all available problems
                                             } deriving (Show)

-- This function simulates the process of learning the LA model from student data.

-- Input: a list of students with their assessment grades.
-- Output: the learned LA model.
learnLAAssessmentModel    :: [Student] -> LAAssessmentModel
learnLAAssessmentModel _  =  undefined

-- This is a placeholder for a list of students' assessment data.
studentAssessmentData  :: [Student]
studentAssessmentData  =  [student1, student2]
  where student1 = undefined
        student2 = undefined

-- Represents an activity that updates the model, such as a new assessment grade.
data StudentActivity = StudentActivity { activityStudentId :: Int
                                       , activityProblemId :: Int
                                       , activityGrade     :: Double
                                       } deriving (Show, Eq)

-- Instance for the Model typeclass. This links our LAAssessmentModel to the
-- generic recommendation framework.
instance Model LAAssessmentModel StudentActivity where
  -- Initializes the model using the student assessment data.
  init    =  learnLAAssessmentModel studentAssessmentData
  -- Updates the model with new data.
  update  =  undefined

-- Predicts the suitability of a given fraction problem for a student.
-- It uses the learning
-- analytics model to determine which problem a student is most ready to work on.
-- A higher score means a more suitable problem.
-- Input: a student, the LA assessment model, and a potential fraction problem.
-- Output: a score representing the suitability of the problem for the student.
predict_problem_suitability                       ::  Student -> LAAssessmentModel -> FractionProblem -> Double
predict_problem_suitability student model problem  =   undefined

-- This service function uses the recommend function from the Recommendation
-- module with our LA assessment model.
-- Input: a student and their current homework assignment.
-- Output: the recommended personalized fraction problem.
service_la  ::  (Student, String) -> LAAssessmentModel -> FractionProblem
service_la  =   recommend

-- | ---------------------------
-- | Recommendation Logic
-- | ---------------------------

-- Instance for the 'Best' typeclass.
-- This implementation follows the logic of the Rodriguez et al. paper,
-- which is to recommend the problem with the highest predicted suitability
-- score based on the student's learning analytics data.
instance Best (Student, String) LAAssessmentModel FractionProblem where
  best (student, _) laModel possibleProblems  =
    let
      -- Map each possible problem to a tuple of (problem, suitability score)
      problemScores   =  map (\p -> (p, predict_problem_suitability student laModel p)) possibleProblems
      -- Find the problem with the maximum suitability score
      (recommendedProblem, _)       =  maximumBy (comparing snd) problemScores
    in
      recommendedProblem