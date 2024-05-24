#3. Hypothesis 3
library(dplyr)
library(mgcv)
######################below is a lot of manipulating, to get the data down to either lone or together individuals.

dominants <- dingomerged[which(dingomerged$Status == "Dominant"),]
dominants$hour <- hour(dominants$DateTime)

dominants <- dominants[-which(dominants$hour < 5),]
dominants <- dominants[-which(dominants$hour > 21),]
#First datetime within 5 mins
dominants$infive <- FALSE

for (i in 1:nrow(dominants)) {
  for (j in 1:nrow(dominants)) {
    # Check if the DateTime values of rows i and j are within 5 minutes of each other
    if (i != j && abs(difftime(dominants$DateTime[i], dominants$DateTime[j], units = "mins")) <= 5) {
      # If they are, set 'infive' to TRUE using logical OR
      dominants$infive[i] <- dominants$infive[i] | TRUE
      dominants$infive[j] <- dominants$infive[j] | TRUE
    }
  }
}

dominants <- dominants%>%
  group_by(Trap)%>%
  mutate(Who = ifelse(infive, "Pair", "Lone"))

##DO RUN need to change MBF1701 row 1, and UOM1707 to LONE-------This is to correct a few errors in the above, a bit messy - do not do like this if you read it. 
dominants$Who[1] <- "Lone" #MBF1701
dominants$Who[227] <- "Lone" #UOM1707
dominants$Who[359] <- "Lone" #UOF1801
dominants$Who[448] <- "Lone" #UOM2002
dominants$Who[70] <- "Lone" #MBF1701
dominants$Who[357] <- "Lone" #UOF1801
dominants$Who[530] <- "Lone" #UOM2012

#Now for neil again, need to remove the dominants we don't know cores for. 
dominants <- dominants[-which(dominants$Individual == "UOF1709"),]
dominants <- dominants[-which(dominants$Individual == "UOM2004"),]
dominants <- dominants[-which(dominants$Individual == "UOF2013"),]
dominants <- dominants[-which(dominants$Individual == "UOM2012"),]

almostthere <- dominants %>%
  group_by(CamTerType, Sex, month, Session) %>%
  summarize(CountLON = sum(Who == "Lone"),
            CountPAIR = sum(Who == "Pair"))%>%
  mutate(Total = CountLON+CountPAIR,
         Proportion = CountLON/Total)



month_mapping <- c("Dec" = 1, "Jan" = 2, "Feb" = 3, "Mar" = 4, "Apr" = 5, "May" = 6)

#now we're assigning traps again. 

almostthere$CamTerType <- factor(almostthere$CamTerType, levels = c("COR", "PER", "OOR"))
almostthere$Sex <- factor(almostthere$Sex, levels = c("M", "F"))
almostthere$months_numeric <- month_mapping[almostthere$month]
almostthere$Session <- factor(almostthere$Session, levels = c("1","2"))

dominantfull <- gam(CountLON~s(months_numeric, by = interaction(Sex,CamTerType), bs = "cr", k = 6)+s(Session, bs = "re"),  data = almostthere, family = 'poisson')

dominantnull <- gam(CountLON~ s(Session, bs = "re"), data = almostthere, family = "poisson")

AIC(dominantfull, dominantnull)
