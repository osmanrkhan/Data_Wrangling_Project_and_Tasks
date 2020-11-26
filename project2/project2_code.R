library("RODBC")
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
myconn<-odbcConnect("dartmouth_qbs181","okhan","okhan@qbs181")
Calls<-sqlQuery(myconn,"select * from Phonecall_Encounter")
# Question 1 -----------------
TempEnc <- Calls$EncounterCode

TempEnc <- TempEnc %>% str_replace_all(c("125060000" = "Clinical Alert",
                   "125060001" = "Health Coaching",
                   "125060002" = "Technical Question",
                   "125060003" = "Administrative",
                   "125060004" = "Other",
                   "125060005" = "Lack of Engagement"))

Calls <- cbind(Calls, TempEnc)
names(Calls)[3] = "EnrollmentGroup"


# Question 2 -----------------

Calls %>%
  group_by(EnrollmentGroup) %>%
   count(EnrollmentGroup)

# Question 3 -----------------

Calls2 <-sqlQuery(myconn,"select * from Phonecall")

# Question 4 -----------------

#CALL TYPE
Calls2$CallType<- Calls2$CallType %>% 
  str_replace_all(c("1" = "Inbound", "2" = "Outbound"))


#CALL OUTCOME
Calls2$CallOutcome<- Calls2$CallOutcome %>% 
  str_replace_all(c("1" = "No Response", "2" = "Left Voice Mail", 
                    "3" = "Successful"))

##4a
Calls2 %>%
  group_by(CallType) %>%
  count(CallType)

##4b
Calls2 %>%
  group_by(CallOutcome) %>%
  count(CallOutcome)

##4c
## Find median call duration for each enrollment group
inner_join(Calls, Calls2, by = c("CustomerId" = "tri_CustomerIDEntityReference")) %>%
  group_by(EnrollmentGroup) %>%
  summarize("Median Call Duration" = median(CallDuration))



##5
Demo<-sqlQuery(myconn,"select * from Demographics")
Conditions<-sqlQuery(myconn,"select * from Conditions")
Texts<-sqlQuery(myconn,"select * from Text")

names(Demo)
names(Conditions)
names(Texts)
head(Texts)

total_wks <- as.Date(Texts$TextSentDate, format = "%Y-%m-%d")

##convert dates into weekly dates for 2016
total_wks <- week(total_wks)


## convert dates into weekly dates for 2017 (54 + i)
total_wks[which(year(as.Date(Texts$TextSentDate, format = "%Y-%m-%d")) == 2017)] = 
  total_wks[which(year(as.Date(Texts$TextSentDate, format = "%Y-%m-%d")) == 2017)] + 54


Texts <- cbind(Texts, total_wks)
names(Texts)[4] = "wk_num"

num_of_weeks <- length(unique(Texts$wk_num))

sort(unique(Texts$wk_num))

inner_join(Demo, Conditions, by = c("contactid" = "tri_patientid")) %>%
  inner_join(., Texts, by = c("contactid" = "tri_contactId")) %>%
  group_by(SenderName, wk_num) %>%
  count() %>%
  group_by(SenderName) %>%
  summarize(avg_texts = mean(n))


##6
inner_join(Demo, Conditions, by = c("contactid" = "tri_patientid")) %>%
  inner_join(., Texts, by = c("contactid" = "tri_contactId")) %>%
  group_by(tri_name, wk_num) %>%
  count() %>%
  group_by(tri_name) %>%
  summarize(avg_texts = mean(n))
  

  
head(Conditions)
head(Demo)
head(Texts)
head(inner_join(Demo, Conditions, by = c("contactid" = "tri_patientid")))
View(inner_join(Demo, Conditions, by = c("contactid" = "tri_patientid")))




