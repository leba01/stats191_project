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

scaledAggData = standardize(aggGameData)
#names(aggGameData)
simpleMLR <- lm(game_score~.-id, data=scaledAggData)
n = nrow(scaledAggData)
scaledReduced <- lm(game_score ~.-id-year_uni, data=scaledAggData)
summary(scaledReduced)
anova(scaledReduced, simpleMLR) #here is pValue notifying year_uni is irrelevant
pdf(file = "plot call to model.pdf")
plot(scaledReduced)
dev.off()
#identify(scaledReduced) #this was for convenience for finding outliers

#this is one of the ones that ethne made
hist(aggGameData$fit_score)

#Getting measures of Influence
fit = fitted(scaledReduced)
residuals = resid(scaledReduced)
standard_res <- rstandard(scaledReduced)
leverage <- hatvalues(scaledReduced)

Cook = standard_res^2/3*leverage/(1-leverage) #RUN THIS WITH THE OUTLIERS IN TO SEE WHY I REMOVED (above),
plot(1:n, Cook)                              # ALSO MANUALLY LOOK THEY MAKE NO SENSE. 100 score with zero everywhere else
#identify(1:n, Cook) #again, for clicking on points
#dev.off()

plot(1:n, standard_res)

#Now need to create player profiles
  #mutate datasets
aggDataMorning <- mutate(scaledAggData, time_game = "morning", coast = "west coast")
aggDataEvening <- mutate(scaledAggData, time_game = "evening", coast = "east coast")
head(aggDataMorning)
head(scaledAggData)
head(aggDataEvening)
playerProfiles <- split(scaledAggData, scaledAggData$id)
playerProfilesMorning <- split(aggDataMorning, scaledAggData$id)
playerProfilesEvening <- split(aggDataEvening, scaledAggData$id)
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
  vari <- var(predicted)
  AvgPredicted <- append(AvgPredicted, list(list(id = i, mean = avg, var = vari)))
  i <- i + 1
}
#print(AvgPredicted)
AvgPredicted <- AvgPredicted[order(-sapply(AvgPredicted, '[[', 2))]
i <- 1
for (entry in playerProfilesMorning) {
  #calculate avg for morning cup
  predicted <- predict(scaledReduced, newdata = entry)
  avg <- mean(predicted)
  vari <- var(predicted)
  AvgPredictedMorning <- append(AvgPredictedMorning, list(list(id = i, mean = avg, var = vari)))
}
AvgPredictedMorning <- AvgPredictedMorning[order(-sapply(AvgPredictedMorning, '[[', 2))]
i <- 1
for (entry in playerProfilesEvening) {
  #calculate avg for morning cup
  predicted <- predict(scaledReduced, newdata = entry)
  avg <- mean(predicted)
  vari <- var(predicted)
  AvgPredictedEvening <- append(AvgPredictedEvening, list(list(id = i, mean = avg, var = vari)))
}
AvgPredictedEvening <- AvgPredictedEvening[order(-sapply(AvgPredictedEvening, '[[', 2))]
i <- 1
length(AvgPlayerPredicted)

#Now figuring out win and lose percentage
prevSzn$diff <- prevSzn$team1_score - prevSzn$team2_score
WinLoss <- glm(winning_team ~ diff, data = prevSzn, family = binomial(link=logit))
summary(WinLoss)

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

