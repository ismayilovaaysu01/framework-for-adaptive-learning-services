{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes #-}

module Nguyen where

import Recommendation

import Data.List (maximumBy)
import Data.Ord (comparing)
--import Data.Function (on)

-- | ---------------------------
-- | Model Implementation
-- | ---------------------------

-- Nguyen implements a Collaborative Filtering (CF) Model
-- This is a placeholder for a CF model.
data CFModel  =  CFModel { modelDataCF :: String -- Placeholder for the actual model data
                         } deriving (Show)

-- This function simulates learning the CF model.
-- Input: a list of students with course grades.
-- Output: the learned CF model.
learnCFModel    :: [Student] -> CFModel
learnCFModel _  =  undefined

dataCF  :: [Student]
-- undefined is better. This is for the purpose of illustration.
dataCF  =  [student1,student2]
  where student1 = undefined
        student2 = undefined

instance Model CFModel l where
  init    =  learnCFModel dataCF
  update  =  undefined

-- Predicts the grade a student would get in a given course using the CF model.
-- Input: a student, a CF model, and a course.
-- Output: a predicted grade for that course.
predict_course_grade                       ::  Student -> CFModel -> Course -> Grade
predict_course_grade student model course  =   undefined

-- A placeholder for the Student's ability parameter.
-- Input: a student.
-- Output: a calculated ability score.
studentAbility    ::  Student -> Double
studentAbility _  =   undefined

-- This service uses the recommend function with a CF model.
service_cf  ::  (Student, Program) -> CFModel -> Course
service_cf  =   recommend

-- Instance for the Collaborative Filtering model.
-- This 'Best' instance implements the logic from the Kyung et al. paper,
-- which recommends the course with the highest predicted grade.
instance Best (Student, Program) CFModel Course where
  best (student, _) cfmodel possibleCourses  =
    let  predictedCourseGrades   =  map (\c -> (c, predict_course_grade student cfmodel c)) possibleCourses
         (recommendedCourse, _)  =  maximumBy (comparing snd) predictedCourseGrades
    in   recommendedCourse



