library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)

# This section imports sample data from RCOA export.
# It renames the titles into something understandable
# It deletes all the sample data, leaving just the titles
rcoa <- read_csv("rcoa.csv", col_names = FALSE)
rcoa <- rcoa %>%
  rename(date = X1, ref = X2, dob=X3, sex=X4,
         asa=X5, priority=X6, speciality=X7, start=X8,
         end=X9, anaesthetic_1=X10, procedure_1=X11,
         procedure_2=X12, procedure_3=X13, incident_1=X14,
         incident_2=X15, incident_3=X16, supervision=X17,
         operation=X18, notes=X19, user1=X20, user2=X21,
         anaesthetic_2=X22, age=X23, site=X24, gmc=X25)

rcoa$start <- as.character(rcoa$start)

# This imports the logbook data from excel
logbook <- read_excel("Anaesthetics_Logbook.xlsx", col_names = TRUE) %>%
  rename(date = Date, sex=Gender, asa=ASA, priority=Priority, speciality=Speciality,
         anaesthetic_1=Airway, anaesthetic_2=`Regional Anaesthetic`, procedure_1=Procedure1,
         supervision=Supervision, operation=Operation, user2=Supervisor, age=Age,
         site=Hospital)

logbook$age <- as.character(logbook$age)
logbook$asa <- as.integer(logbook$asa)

logbook <- logbook %>%
  separate(date, into = c("year", "month", "day")) %>%
  unite(date, day, month, year, sep="/")


rcoa <- rcoa %>%
  filter(gmc==0000000)

logbook$Timing[logbook$Timing == "Day"] <- "08:00"
logbook$Timing[logbook$Timing == "Evening"] <- "18:30"
logbook$Timing[logbook$Timing == "Night"] <- "00:30"

logbook <- logbook %>%
  rename(start = Timing)



logbook <- bind_rows(rcoa, logbook) %>%
  select(-`Airway-Code`:-`Codes:`)

# This exports the reformatted logbook, ready to be imported into the anaesthetic logbook
write_csv(logbook, "logbook.csv", col_names = TRUE)

