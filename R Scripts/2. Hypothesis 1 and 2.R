#2. Hypothesis 1 and 2 testing
#Formatting for hypothesis 1 and 2, and modelling. 

library(dplyr)
library(mgcv)

#First we're going to sort it by month; 
nounknown <- dingomerged[-which(dingomerged$Pack == "UNK"), ]


Lookingatpackandpupmonth <- nounknown %>%
  group_by(Individual, Session, month)%>%
  summarize(CountCOR = sum(CamTerType == "COR"),
            CountPER = sum(CamTerType == "PER"), 
            CountOOR = sum(CamTerType == "OOR"),
            CountOTH = sum(CountPER+CountOOR),
            CountOTH2 = sum(CountCOR+CountOOR),
            Status = Status,
            Sex = Sex,
            PupsTot = PupsTot)%>%
  distinct()

#We are here extending dataset so that each month and each zone has a value. It's count data, so zeroes need to be present. 

Lookingatpackandpupmonth <- Lookingatpackandpupmonth %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = starts_with("Count"), names_to = "Zone", values_to = "Count") %>%
  mutate(Zone = factor(Zone, levels = c("CountOOR", "CountCOR", "CountPER")),
         Zone = if_else(Zone == "CountOOR", "OOR", if_else(Zone == "CountCOR", "Core", "Peripheral")))

####Now adding more variables we need. 

Lookingatpackandpupmonth <- Lookingatpackandpupmonth[-which(is.na(Lookingatpackandpupmonth$Zone)),]
Lookingatpackandpupmonth<- Lookingatpackandpupmonth%>%
  mutate(StatSex = interaction(Status,Sex, drop = TRUE))
Lookingatpackandpupmonth$Status <- as.factor(Lookingatpackandpupmonth$Status)
Lookingatpackandpupmonth$month <- as.factor(Lookingatpackandpupmonth$month)



Lookingatpackandpupmonth <- Lookingatpackandpupmonth%>%
  mutate(Dummy = str_replace(Dummy,"Dominant.F", "DominantF"),
         Dummy = str_replace(Dummy,"Dominant.M", "DominantM"),
         Dummy = str_replace(Dummy,"SubDominant.F", "SubDominantF"),
         Dummy = str_replace(Dummy,"SubDominant.M", "SubDominantM"))

#Finally, remove the packs for which we don't have the collar data: These are yaccaba and sandbar packs. 

OnlyCollar <- Lookingatpackandpupmonth[-which(Lookingatpackandpupmonth$Individual == "UOF1709"),]
OnlyCollar <- OnlyCollar[-which(OnlyCollar$Individual == "UOM2004"),]
OnlyCollar <- OnlyCollar[-which(OnlyCollar$Individual == "UOF2013"),]
OnlyCollar <- OnlyCollar[-which(OnlyCollar$Individual == "UOM2012"),]



#####Define months in numeric


month_mapping <- c("Dec" = 1, "Jan" = 2, "Feb" = 3, "Mar" = 4, "Apr" = 5, "May" = 6)

#Asssign factors to all variables
OnlyCollar$month <- factor(OnlyCollar$month, levels = c("Dec", "Jan", "Feb","Mar", "Apr", "May"))


OnlyCollar$months_numeric <- month_mapping[OnlyCollar$month]

#This is for plotting
OnlyCollar$Dummy <- factor(OnlyCollar$Dummy, levels = c("SubDominantM", "SubDominantF","DominantM", "DominantF"))

OnlyCollar$Status <- factor(OnlyCollar$Status, levels = c("Dominant", "SubDominant"))

OnlyCollar$Sex <- factor(OnlyCollar$Sex, levels = c("F", "M"))

OnlyCollar$Zone <- factor(OnlyCollar$Zone, levels = c("Core", "Peripheral", "OOR"))


#We've now got data in correct format, we can run our three models

#First model all together
AllTogether <- gam(Count~
                    s(months_numeric,k=6, bs = "cr")+
                    s(Session, bs = "re"),
                  family = poisson(),
                  data = OnlyCollar)

#Second model varying by individual
VaryIndiv <- gam(Count~
                   s(months_numeric,by = interaction(Status,Sex,Zone),k=6, bs = "cr")+
                   s(Session, bs = "re"),
                 family = poisson(),
                 data = OnlyCollar)

Nullhyp1 <- gam(Count  ~ 
                  s(Session, bs = "re"), data = OnlyCollar, family = poisson)

#Compare AICs
AIC(AllTogether, VaryIndiv,Nullhyp1)

#end, all done. 

