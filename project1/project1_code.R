library("RODBC")
library(dplyr)
library(tidyverse)
library(lubridate)

myconn<-odbcConnect("dartmouth_qbs181","okhan","okhan@qbs181")
IC_Demo<-sqlQuery(myconn,"select * from Demographics")

# Question 1 -----------------------------------------
names(IC_Demo)
names(IC_Demo) = c("ID", "Gender1", "Age", "Parent", "ImagineCareEnrollStatus", 
                   "State", "EmailSentDate", "CompleteDate", "Gender2")
names(IC_Demo)

EnrollmentTime = interval(mdy(IC_Demo$EmailSentDate), mdy(IC_Demo$CompleteDate)) %/% days(1)
IC_Demo <- cbind(IC_Demo, EnrollmentTime)

# Question 2 -------------------------------------------

EnrollmentStatus <- IC_Demo$ImagineCareEnrollStatus
#a
EnrollmentStatus[which(IC_Demo$ImagineCareEnrollStatus == "167410011")] <- "Complete"
#b
EnrollmentStatus[which(IC_Demo$ImagineCareEnrollStatus == "167410001")] <- "Email sent"
#c
EnrollmentStatus[which(IC_Demo$ImagineCareEnrollStatus == "167410004")] <- "Non responder"
#d
EnrollmentStatus[which(IC_Demo$ImagineCareEnrollStatus == "167410005")] <- "Facilitated enrollment"
#e
EnrollmentStatus[which(IC_Demo$ImagineCareEnrollStatus == "167410002")] <- "Incomplete Enrollments"
#f
EnrollmentStatus[which(IC_Demo$ImagineCareEnrollStatus == "167410003")] <- "Opted Out"
#g
EnrollmentStatus[which(IC_Demo$ImagineCareEnrollStatus == "167410000")] <- "Unprocessed"
#h
EnrollmentStatus[which(IC_Demo$ImagineCareEnrollStatus == "167410006")] <- "Second email sent"


IC_Demo <- cbind(IC_Demo, EnrollmentStatus)

# Question 3 -------------------------------------------

typeof(IC_Demo$Gender1)
Gender <- IC_Demo$Gender1
#a
Gender[which(IC_Demo$Gender1 == "2")] <- "female"
#b
Gender[which(IC_Demo$Gender1 == "1")] <- "male"
#c
Gender[which(IC_Demo$Gender1 == "167410000")] <- "other"
#d
Gender[which(IC_Demo$Gender1 == "null")] <- "Unknown"

IC_Demo <- cbind(IC_Demo, Gender)


# Question 4 -------------------------------------------
Agegroup <- IC_Demo$Age

Agegroup[which(IC_Demo$Age<25)] <- "0-25"
#b
Agegroup[which(25< IC_Demo$Age & IC_Demo$Age <51)] <- "26-50"
#c
Agegroup[which(50 < IC_Demo$Age & IC_Demo$Age < 76)] <- "51-75"
#d
Agegroup[which(IC_Demo$Age > 75)] <- "Above 75"


IC_Demo[sample(nrow(IC_Demo), 10), ]






