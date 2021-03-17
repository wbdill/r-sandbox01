# Desc: Reads Excel file and does data consistency checks
# Auth: bdill 2021-03-16

#install.packages("tidyverse")
#install.packages("readxl")
library(tidyverse)
library(readxl)

rm(list = ls())
xlpath <- "C:/Users/bdill/Downloads/Fairfield_Import_Data_2020.2021.2.xlsx"

acadperiods <- read_xlsx(xlpath, sheet = "AcademicPeriods")
schools <- read_xlsx(xlpath, sheet = "Schools")
models <- read_xlsx(xlpath, sheet = "Models")
divisions <- read_xlsx(xlpath, sheet = "Divisions", )
teachers <- read_xlsx(xlpath, sheet = "Teachers")
students <- read_xlsx(xlpath, sheet = "Students")
courses <- read_xlsx(xlpath, sheet = "Courses")
classes <- read_xlsx(xlpath, sheet = "Classes")
enrollment <- read_xlsx(xlpath, sheet = "Enrollment")

#----- Periods / Schools -----
acadperiods %>% group_by(PeriodName) %>% count() %>% filter(n > 1)        # PeriodName must be unique
schools %>% group_by(StateSchoolCode) %>% count() %>% filter(n > 1)       # StateSchoolCode must be unique
#----- Models -----
models %>% group_by(ModelID) %>% count() %>% filter(n > 1)                # ModelID must be unique
models %>% group_by(ModelName) %>% count() %>% filter(n > 1)              # ModelName must be unique
#----- Divisions -----
divisions %>% group_by(DivisionID) %>% count() %>% filter(n > 1)          # DivisionID must be unique
divisions %>% group_by(DivisionName) %>% count() %>% filter(n > 1)        # DivisionName must be unique
#----- Teachers -----
teachers %>% group_by(TeacherLicenseNumber) %>% count() %>% filter(n > 1) # TLN must be unique
teachers %>% group_by(StateEmployeeID) %>% count() %>% filter(n > 1)      # StateEmployeeID must be unique
teachers %>% group_by(InternalTeacherID) %>% count() %>% filter(n > 1)    # InternalTeacherID must be unique
teachers %>% anti_join(schools, by = "StateSchoolCode")                   # StateSchoolCode must exist in Schools
#----- Students -----
students %>% anti_join(schools, by = "StateSchoolCode")                   # StateSchoolCode must exist in schools
students %>% group_by(StateStudentCode) %>% count() %>% filter(n > 1)     # StateStudentCode must be unique
students %>% group_by(InternalStudentID) %>% count() %>% filter(n > 1)    # InternalStudentID must be unique
#----- Courses -----
courses %>% anti_join(models, by = "ModelName")                                 # ModelName must exist in Models
course_dupes <- courses %>% group_by(CourseCode) %>% count() %>% filter(n > 1)  # CourseCode must be unique
if(nrow(course_dupes) > 0) { courses %>% inner_join(course_dupes, by = "CourseCode") }
#----- Classes -----
classes %>% anti_join(schools, by = "StateSchoolCode")                          # StateSchoolCode must exist in schools
classes %>% anti_join(acadperiods, by = c("AcademicPeriodName" = "PeriodName")) # AcademicPeriodName must exist in AcademicPeriods
classes %>% anti_join(courses, by = "CourseCode")                               # CourseCode must exist in Courses
classes %>% anti_join(teachers, by = "TeacherLicenseNumber")                    # TLN must exist in Teachers
classes_dupes <- classes %>% group_by(LocalCourseCode) %>% count() %>% filter(n > 1)  # LocalCourseCode must be unique
if(nrow(classes_dupes) > 0) { classes %>% inner_join(classes_dupes, by = "LocalCourseCode")}
#----- Enrollment -----
enrollment %>% anti_join(schools, by = "StateSchoolCode")       # StateSchoolCode must exist in Schools
enrollment %>% anti_join(classes, by = "LocalCourseCode")       # LocalCourseCode must exist in Classes
enrollment %>% anti_join(students, by = "StateStudentCode")     # StateStudentCode must exist in Students
enrollment %>% anti_join(students, by = "InternalStudentID")    # InternalStudentID must exist in Students
