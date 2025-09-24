{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes #-}

module Chen where

-- We need a few standard libraries for list manipulation and comparison.

import Recommendation

import Data.List (minimumBy)
import Data.Ord (comparing)
--import Data.Function (on)

-- Chen implements an Item Response Theory (IRT) Model
-- This is a placeholder for an IRT model.
data IRTModel = IRTModel {  courseParameters :: [(Course, Double, Double)]
                         -- The model contains parameters for each course, such as difficulty and discrimination.
                         } deriving (Show)

-- This function simulates learning the IRT model.
-- Input: a list of enrolled students.
-- Output: the learned IRT model with course parameters.
learnIRTModel    ::  [Student] -> IRTModel
learnIRTModel _  =   undefined

instance Model IRTModel l where
  init    =  learnIRTModel undefined
  update  =  undefined

-- A placeholder for the Student's ability parameter.
-- Input: a student.
-- Output: a calculated ability score.
studentAbility    ::  Student -> Double
studentAbility _  =   undefined

-- Predicts the probability of a student passing a course using the IRT model.
-- Input: a student, an IRT model, and a course.
-- Output: a predicted probability of success (as a Grade).
predict_course_outcome                       ::  Student -> IRTModel -> Course -> Grade
predict_course_outcome student model course  =   undefined

-- This service uses the recommend function with an IRT model.
service_irt  ::  (Student, Program) -> IRTModel -> Course
service_irt  =   recommend

-- Instance for the Item Response Theory model.
-- This 'Best' instance implements the logic from the Chen et al. paper,
-- which recommends the course with a probability closest to the optimal.
instance Best (Student, Program) IRTModel Course where
  best (student, _) mlmodel possibleCourses =
    let  optimalProbability     =  undefined
         predictedCourseGrades  =  map (\c -> (c, predict_course_outcome student mlmodel c)) possibleCourses
         (recommendedCourse, _)  =  minimumBy (comparing (\(_, (Grade p)) -> abs (p - optimalProbability))) predictedCourseGrades
    in   recommendedCourse



