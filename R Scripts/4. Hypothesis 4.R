#Hypothesis 4. 
library(dplyr)
library(nnet)
#First need to do some more processing: 

Lookingatpackandpup <- nounknown %>%
  group_by(Individual, Session)%>%
  summarize(CountCOR = sum(CamTerType == "COR"),
            CountPER = sum(CamTerType == "PER"), 
            CountOOR = sum(CamTerType == "OOR"),
            CountOTH = sum(CountPER+CountOOR),
            CountOTH2 = sum(CountCOR+CountOOR),
            Status = Status,
            Sex = Sex,
            PupsTot = PupsTot)%>%
  distinct()


##Add interaction variable- just in case, probs don't need
Lookingatpackandpup<- Lookingatpackandpup %>%
  mutate(StatSex =interaction(Status, Sex, drop = TRUE))

#Assign factors
Lookingatpackandpup$StatSex <- as.factor(Lookingatpackandpup$StatSex)
Lookingatpackandpup$Status <- as.factor(Lookingatpackandpup$Status)
Lookingatpackandpup$PupsTot <- as.numeric(Lookingatpackandpup$PupsTot)

#Rename 
Lookingatpackandpup <- Lookingatpackandpup %>%
  rename('Core' = CountCOR,
         'Peripheral' = CountPER,
         'Out_Of_Range' = CountOOR)

#Again removing individuals that don't have the collar data for their pack

#removing those individuals again: 
Lookingatpackandpup <- Lookingatpackandpup[-which(Lookingatpackandpup$Individual == "UOF1709"),]
Lookingatpackandpup <- Lookingatpackandpup[-which(Lookingatpackandpup$Individual == "UOM2004"),]
Lookingatpackandpup <- Lookingatpackandpup[-which(Lookingatpackandpup$Individual == "UOF2013"),]
Lookingatpackandpup <- Lookingatpackandpup[-which(Lookingatpackandpup$Individual == "UOM2012"),]


fullnumltinom <- multinom(cbind(Core,Peripheral,Out_Of_Range) ~Status*PupsTot, data = Lookingatpackandpup)
nullmultinom <- multinom(cbind(Core,Peripheral, Out_Of_Range)~1,data = Lookingatpackandpup)
AIC(fullnumltinom,nullmultinom)

#Full better

