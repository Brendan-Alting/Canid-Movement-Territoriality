###1 Arrange data to get datasets suitable for models. 
library(dplyr)
library(lubridate)
#First we will read in the dataframe which contains information from each individual- including the status of each trap within their range (Core, Peripheral, Out Of Range) (COR,PER,OOR)

dingostatuses <- read.csv(file = "Raw Data/dingostatuses.csv", header = T)

#Also read in detections dataset, which contains all dingo detections from both years

dingodetections <- read.csv(file = "Raw Data/DingoDetections.csv", header = T)

#now need to merge some traps, as they were the same location, just moved slightly
trap_mapping <- c("PS24" = "PS4", "PS23" = "PS7", "PS25" = "PS10", "PS27" = "PS22")

dingodetections <- dingodetections %>%
  mutate(Trap = ifelse(Trap %in% names(trap_mapping), trap_mapping[Trap], Trap))

#Remove PS26, as it is only active for year 2
dingodetections <- dingodetections[-which(dingodetections$Trap == "PS26"),]


#Make column called CamTerType
PS_columns <- paste0("PS", 1:22)

dingostatuses_duplicated <- dingostatuses %>%
  gather(key = "Trap", value = "CamTerType", PS_columns)

#Now we are going to combine the two dataframes, taking the right information from each. 

dingomerged <- left_join(dingodetections, dingostatuses_duplicated %>% select(Status, Pack, Sex, Individual, Session,PupsTot, CamTerType, Trap), by = c("Individual" = "Individual", "Session" = "Session", "Trap" = "Trap"))




#now remove unidentified individuals:
dingomerged <- dingomerged[-which(dingomerged$Individual == "Unidentifiable"),]


#Now we are going to subset down to correct dates

#Using just 28day survey periods- so approx 1 month. Just do total detections on cameras. 
dingomerged$Date <- as.Date(dingomerged$Date)
#First get dates in month and day (Same time period, different year).  

dingomerged <- dingomerged %>%
  mutate(DateNoYear = format(Date, "%m-%d"))

#Filter to correct dates
dingomerged <- dingomerged %>%
  filter((Date >= as.Date("2021-12-05") & Date <= as.Date("2022-05-28")) |
           (Date >= as.Date("2022-12-05") & Date <= as.Date("2023-05-28")))

#Get dates and assign a categorical code-this are equal number of days, so the counts are as a ratio. 
dingomerged <- dingomerged %>%
  mutate(month = case_when(
    between(Date, as.Date("2021-12-05"), as.Date("2022-01-02")) |
      between(Date, as.Date("2022-12-05"), as.Date("2023-01-02")) ~ "Dec",
    between(Date, as.Date("2022-01-03"), as.Date("2022-01-30")) |
      between(Date, as.Date("2023-01-03"), as.Date("2023-01-30")) ~ "Jan",
    between(Date, as.Date("2022-01-31"), as.Date("2022-02-27")) |
      between(Date, as.Date("2023-01-31"), as.Date("2023-02-27")) ~ "Feb",
    between(Date, as.Date("2022-02-28"), as.Date("2022-03-27")) |
      between(Date, as.Date("2023-02-28"), as.Date("2023-03-27")) ~ "Mar",
    between(Date, as.Date("2022-03-28"), as.Date("2022-04-24")) |
      between(Date, as.Date("2023-03-28"), as.Date("2023-04-24")) ~ "Apr",
    between(Date, as.Date("2022-04-25"), as.Date("2022-05-22")) |
      between(Date, as.Date("2023-04-25"), as.Date("2023-05-22")) ~ "May",
    between(Date, as.Date("2022-05-23"), as.Date("2022-06-19")) |
      between(Date, as.Date("2023-05-23"), as.Date("2023-06-19")) ~ "Jun",
    TRUE ~ NA_character_  # In case none of the conditions are met
  ))

#Not enough dates in June. 
dingomerged <- dingomerged[-which(dingomerged$month == "Jun"),]

#Two incorrect records- just change them

dingomerged <- dingomerged%>%
  filter(!(Individual == "UOM1707" & Trap == "PS5"))

dingomerged <- dingomerged%>%
  filter(!(Individual == "UOM2007" & Date == "2023-03-05
"))

#end