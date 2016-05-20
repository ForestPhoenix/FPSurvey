module Query.Survey where

import Opaleye

import Model

querySurvey :: Query (SectionColumns)
querySurvey = queryTable sectionTable
