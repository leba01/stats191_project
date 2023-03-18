#Stats 191 code

setwd("~/Desktop/Stats 191 Final Project") #set this to whatever it is in your computer
#SET UP LIBRARIES
#install.packages("glmnet")
#install.packages("datawizard")
#install.packages("pls")
library(ggplot2)
library(dplyr)
library(datawizard)
library(glmnet)
library(pls)

#PROCESS RAW DATA
all_games <- list.files(pattern = "game_.*.csv")
gameData <- list()
for (game in all_games) {
  g <- read.csv(game, header = TRUE)
  #g_std <-as.data.frame(scale(g))
  gameData[[game]] <- g
}
prevSzn <- read.csv("./previous_season_results.csv", header=TRUE)
sznMatchups <- read.csv("./season_match_up.csv", header=TRUE)
#combine all data, assuming independence here NOTE THIS IS AN IMPORTANT ASSUMPTION
aggGameDataWithOutliers <- do.call(rbind, gameData)

#remove top 3 outliers
#GAME 5 ROW 130 IS A PROBLEM 
#GAME 3 ROW 930 IS A PROBLEM
#GAME 3 ROW 67

aggGameData <- aggGameDataWithOutliers[-c(3067, 3920, 5130), ] #COMMENT THIS OUT TO RUN WITH OUTLIERS AKA GET COOK PLOT FROM HERE
#aggGameData <- aggGameDataWithOutliers #only one of these two lines should be active at once
#aggGameData <- mutate(aggGameData, meals_prior = meals_prior^(2))
scaledAggData <- aggGameData #standardize(aggGameData)
#names(aggGameData)
simpleMLR <- lm(game_score~.-id, data=scaledAggData)
n = nrow(scaledAggData)
scaledReduced <- lm(game_score ~.-id-year_uni, data=scaledAggData)
summary(scaledReduced)
anova(scaledReduced, simpleMLR) #here is pValue notifying year_uni is irrelevant
#pdf(file = "plot call to model.pdf")
plot(scaledReduced)
#identify(plot(scaledReduced))

#dev.off()
#identify(scaledReduced) #this was for convenience for finding outliers
#this is one of the ones that ethne made
hist(aggGameData$fit_score)

#Getting measures of Influence
fit = fitted(scaledReduced)
residuals = resid(scaledReduced)
standard_res <- rstandard(scaledReduced)
leverage <- hatvalues(scaledReduced)
temp <- plot(fit, residuals, main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
identify(scaledReduced$fit, scaledReduced$residuals)
Cook = standard_res^2/3*leverage/(1-leverage) #RUN THIS WITH THE OUTLIERS IN TO SEE WHY I REMOVED (above),
plot(1:n, Cook)                              # ALSO MANUALLY LOOK THEY MAKE NO SENSE. 100 score with zero everywhere else
#identify(1:n, Cook) #again, for clicking on points
#dev.off()

plot(1:n, standard_res)

#Now need to create player profiles
  #mutate datasets
aggDataMorning <- mutate(scaledAggData, time_game = "morning", coast = "west coast")
aggDataEvening <- mutate(scaledAggData, time_game = "evening", coast = "east coast")

regFixed <- mutate(scaledAggData, per_train_before = 0.95)
morningFixed <- mutate(aggDataMorning, per_train_before = 0.95)
eveningFixed <-eveningFixed(aggDataEvening, per_train_before = 0.95)

#normal
playerProfiles <- split(scaledAggData, scaledAggData$id)
playerProfilesMorning <- split(aggDataMorning, scaledAggData$id)
playerProfilesEvening <- split(aggDataEvening, scaledAggData$id)

#fixing percent training sessions
#playerProfiles <- split(regFixed, regFixed$id)
#playerProfilesMorning <- split(morningFixed, morningFixed$id)
#playerProfilesEvening <- split(eveningFixed, eveningFixed$id)

length(playerProfiles)
names(scaledReduced)
#Now run regressions and take avg
AvgPredicted <- list()
AvgPredictedMorning <- list()
AvgPredictedEvening <- list()
#for (i in 1:length(playerProfiles[1:5])) {
i <- 1
for (entry in playerProfiles) {
  #calculate avg for reg szn
  predicted <- predict(scaledReduced, newdata = entry)
  #print("GOT HERE")
  avg <- mean(predicted)
  stddev <- sd(predicted)
  AvgPredicted <- append(AvgPredicted, list(list(id = i, mean = avg, sd = stddev)))
  i <- i + 1
}
#print(AvgPredicted)
AvgPredicted <- AvgPredicted[order(-sapply(AvgPredicted, '[[', 2))]
i <- 1
for (entry in playerProfilesMorning) {
  #calculate avg for morning cup
  predicted <- predict(scaledReduced, newdata = entry)
  avg <- mean(predicted)
  stddev <- var(predicted)
  AvgPredictedMorning <- append(AvgPredictedMorning, list(list(id = i, mean = avg, sd = stddev)))
  i <- i + 1
}
AvgPredictedMorning <- AvgPredictedMorning[order(-sapply(AvgPredictedMorning, '[[', 2))]
i <- 1
for (entry in playerProfilesEvening) {
  #calculate avg for morning cup
  predicted <- predict(scaledReduced, newdata = entry)
  avg <- mean(predicted)
  stddev <- sd(predicted)
  AvgPredictedEvening <- append(AvgPredictedEvening, list(list(id = i, mean = avg, sd = stddev)))
  i <- i + 1
}
AvgPredictedEvening <- AvgPredictedEvening[order(-sapply(AvgPredictedEvening, '[[', 2))]
i <- 1
length(AvgPredictedEvening)

#Lets find the rosters
#Prioritize West Coast Cup aka morning team
WestCoastRoster <- AvgPredictedMorning[1:10]
WestCoastRosterIds <- lapply(WestCoastRoster, function(x) x$id)
WestCoastRosterIds

#Now finding best of rest for East Coast Cup
EastPotential <- lapply(AvgPredictedEvening, function(x) {
  if (!(x$id %in% WestCoastRosterIds)) {x}
})
EastPotential <- Filter(function(x) !is.null(x), EastPotential)
EastCoastRoster <- EastPotential[1:10]
EastCoastRosterIds <- lapply(EastCoastRoster, function(x) x$id)

#Now finding best of rest for Regular Season
RegPotential <- lapply(AvgPredicted, function(x) {
  if (!(x$id %in% WestCoastRosterIds) && !(x$id %in% EastCoastRosterIds)) {x}
})
RegPotential <- Filter(function(x) !is.null(x), RegPotential)
RegFrame <- data.frame(do.call(rbind, RegPotential))
RegFrame$min <- as.numeric(RegFrame$mean) - (2 * as.numeric(RegFrame$sd))

RegRoster <- head(RegFrame, 10)
RegRosterIds <- lapply(RegRoster, function(x) x$id)
RegRosterTeamScoreMax <- lapply(list(RegRoster), function(x) x$min)
TeamScore <- mean(unlist(RegRosterTeamScoreMax))
# THIS ABOVE IS FOR MAXIMIZING TEAM SCORE
ByMinScore <- RegFrame[order(-RegFrame$min),]
MinLossRoster <- head(ByMinScore, 10)
MinLossIds <- MinLossRoster$id
MinLossRosterMeans <- lapply(list(MinLossRoster), function(x) x$mean)
print(unlist(MinLossIds))
#TeamScore <- mean(unlist(MinLossRosterMeans))
#above this is for minimizing loss team

cbind(RegRosterIds, EastCoastRosterIds, WestCoastRosterIds)
#Now figuring out win and lose percentage
prevSzn$diff <- prevSzn$team1_score - prevSzn$team2_score
WinLoss <- glm(winning_team ~ diff, data = prevSzn, family = binomial(link=logit))
summary(WinLoss)

sznDiffs = list()
for (x in sznMatchups[2:21]) {
  diff = TeamScore - x
  sznDiffs <- append(sznDiffs, diff)
}
df <- data.frame(diff = unlist(sznDiffs))

predict(WinLoss, df, type="response")

#Miscellaneous
pairwise <- pairs(quant_variables, alpha = 0.5, cex = 0.1)

residuals <- as.data.frame(scaledReduced$residuals)
scaledAggDataWithResiduals <- cbind(scaledAggData, residuals)
colnames(scaledAggDataWithResiduals)[colnames(scaledAggDataWithResiduals) == "scaledReduced$residuals"] <- "residuals"
#quantitative
pdf(file = "residualPlots.pdf")
plot(scaledAggDataWithResiduals$per_train_before, scaledAggDataWithResiduals$residuals, xlab = "per_train_before", ylab = "Residuals", pch=20)
plot(scaledAggDataWithResiduals$fit_score, scaledAggDataWithResiduals$residuals, xlab = "fit_score", ylab = "Residuals", pch=20)
plot(scaledAggDataWithResiduals$num_strategy, scaledAggDataWithResiduals$residuals, xlab = "num_strategy", ylab = "Residuals", pch=20)
plot(scaledAggDataWithResiduals$hrs_sleep, scaledAggDataWithResiduals$residuals, xlab = "hrs_sleep", ylab = "Residuals", pch=20)
plot(scaledAggDataWithResiduals$meals_prior, scaledAggDataWithResiduals$residuals, xlab = "meals_prior", ylab = "Residuals", pch=20)
#qualitative

ggplot(scaledAggDataWithResiduals, aes(x = coast, y = residuals)) +
  geom_point()
ggplot(scaledAggDataWithResiduals, aes(x = bird_owl, y = residuals)) +
  geom_point()
ggplot(scaledAggDataWithResiduals, aes(x = ath_hx, y = residuals)) +
  geom_point()
ggplot(scaledAggDataWithResiduals, aes(x = time_game, y = residuals)) +
  geom_point()
dev.off()

