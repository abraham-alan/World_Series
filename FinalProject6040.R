library(Lahman)

library(dplyr)

data("Teams")
head(Teams)
table(Teams$LgWin)
table(Teams$DivWin)
table(Teams$WSWin)
fix(newTeams)
# remove years without a World Series and teams that did not make it to the WS that year
newTeams <- filter(Teams, LgWin == "Y" & yearID > 1904)
# create predictions for Wins in WS based on other categories
# create matrix to see any relationships with WS wins
names(newTeams)
# let's remove non-applicable columns- leageID, team ID, franchID, DivID, Rank, DivWin, 
# WCWin, LgWin, park, teamIDBR and the remaining ones following
newTeams <- newTeams[,-c(2:6,11:13,42,46:48)]
# reorder the columns so that characters are grouped first 
newTeams <- newTeams[,c(1,33,6,2:5,7:32,34:36)]

names(newTeams)
#let's rename the colnames for better recognition
colnames(newTeams) <- c("yearID", "name", "WSWin", "Games", "GameHome","Wins","Losses", 
                        "Runs","AtBats", "Hits", "Doubles", "Triples", "Homeruns",
                        "BatterWalks","BatterStrikeOuts", "StolenBases",
                        "CaughtStealing",
                        "HitByPitch","sacrificeFlies",
                        "OpponentRuns", "EarnedRunsAllowed", "EarnedRunAvg",
                        "CompleteGames", "Shutouts", "Saves","InningsPitched",
                        "HitsAllowed", "HRallowed", "WalksAllowed", "PitcherSO",
                        "Errors", "DoublePlays", "FieldingPercentage", "Attendance",
                        "ThreeYrParkFactorBatter",
                        "ThreeYrParkFactorPitcher")

# now let's see correlations
# first convert Wins/lose to dummy variables 0,1

newTeams$WSWinDummy <- ifelse(newTeams$WSWin == "N",0,1)
cor(newTeams[,-c(1:3)])
# many na's
sum(is.na(newTeams))
# 316 na's

colSums(is.na(newTeams))
# 7 columns have NAs
# lets remove columns of CaughtStealing (54),HitByPitch (120), and sacrificeFlies (132) 
newTeams <- newTeams[,-c(17:19)]
#impute missing values for WSWin with a simple NO because they lost in those years
newTeams$WSWin[which(is.na(newTeams$WSWin))] <- "N"
# now add those values again to the dummy variable of WSWins
newTeams$WSWinDummy <- ifelse(newTeams$WSWin == "N",0,1)


# let's impute the average value so as not to lose so much data in the model

newTeams$Attendance[which(is.na(newTeams$Attendance))] <- 
  mean(newTeams$Attendance, na.rm = TRUE)
newTeams$BatterStrikeOuts[which(is.na(newTeams$BatterStrikeOuts))] <- 
  mean(newTeams$BatterStrikeOuts, na.rm = TRUE)
newTeams$GameHome[which(is.na(newTeams$GameHome))] <- 
  mean(newTeams$GameHome, na.rm = TRUE)

# let's check if that worked
sum(is.na(newTeams))
# 0 na's!!

#lets see the variables over the years for losers and winners
library(ggplot2)
ggplot(newTeams, aes(x=yearID, y=Games)) + geom_line()

#lets remove 1918,1919,1981,1995,too few games those years, must've been a half season

newTeams <- newTeams[!(newTeams$yearID %in% c(1918, 1919, 1981,1995)),]
# let's plot it again now
ggplot(newTeams, aes(x=yearID, y=Games)) + geom_line()
# wow big increase in 1961 and on

ggplot(newTeams, aes(x=yearID, y=AtBats)) + geom_line()
# steady increase in atbats til 1980s
ggplot(newTeams, aes(x=yearID, y=Wins, colour=WSWin)) + geom_line()

# very interesting! wins are not necessarily more for the WS winners
ggplot(newTeams, aes(x=yearID, y=Losses, colour=WSWin)) + geom_line()
# same goes for losses

ggplot(newTeams, aes(x=yearID, y=Hits)) + geom_line()
# big increase in hits from 1920 and on
ggplot(newTeams, aes(x=yearID, y=Runs, colour=WSWin)) + geom_line()


table(newTeams$WSWin)
# 112 to 110
#let's look at correlations
cor(newTeams[,-c(1:3)])
cor(newTeams$WSWinDummy,newTeams[,c(4:33)] )
library(corrplot)
corrplot(cor(newTeams[,-c(1:3)]), method="ellipse",  order = "hclust", tl.col='black',
         tl.cex=.75)
?corrplot
names(newTeams)
# walks allowed, earned run average, earnedRuns allowed, stolen bases,
# park factors isn't making sense so let's remove
newTeams1 <- newTeams[,-c(16,18:19,26,32:33)]
corrplot(cor(newTeams1[,-c(1:3)]), method="ellipse",  order = "hclust", tl.col='black',
         tl.cex=.65)
# create decision tree
library(rpart)
library(partykit)
library(randomForest) 
library(gbm)
library(caret) 
library(rattle)
library(rpart.plot)
library(RColorBrewer)
set.seed(111)
trainSample <- createDataPartition(y = newTeams1$WSWin, p= 0.7, list = FALSE)
newTeamsTrain <- newTeams1[trainSample,]
newTeamsTest <- newTeams1[-trainSample,]
#remove unwanted variables for tree like yearID, TeamName, and DummyWSWins
newTeamsTrain1 <- newTeamsTrain[,-c(1:2,28)]
names(newTeamsTrain1)

set.seed(111)
mytree <- rpart(WSWin ~., data = newTeamsTrain1, method = "class")
fancyRpartPlot(mytree, cex= .7, caption = "Decision Tree Predicting a World Series Win")

mytree
plotcp(mytree)
abline(v = 4, col = "red", lty = "dashed")
mytree$cptable
mytree$variable.importance
cp = min(mytree$cptable[4,])
set.seed(111)

mytreePruned <- prune(mytree, cp = cp)
fancyRpartPlot(mytreePruned,cex= .7, caption = "Pruned Tree")
mytreePruned
plotcp(mytreePruned)
plot(as.party(mytree))
plot(as.party(mytreePruned))

rpart.plot(mytreePruned, type= 4, cex = 0.5)
#
summary(mytree)

#now let's test the test data with this tree...

mytreeTest <- predict(mytree, newdata = newTeamsTest, type = "class")
table(mytreeTest, newTeamsTest$WSWin)
#test results  N  Y
#           N 16 21
#           Y 17 12
(12+16)/66
# 0.42 very poor


mytreeTestPredictPruned <- predict(mytreePruned, newdata = newTeamsTest, type = "class")
table(mytreeTestPredictPruned, newTeamsTest$WSWin)
(16+15)/66
#  0.47 
#pruned tree was better


# now let's try Randon Forest

library(randomForest)
sqrt(28) # =5
set.seed(123)
rf.Teams <- randomForest(WSWin ~., data = newTeamsTrain1)

# keep on receiving error. stackoverflow says I need to convert to factor

?randomForest
newTeamsTrain1$WSWin <- as.factor(newTeamsTrain1$WSWin)
set.seed(123)
rf.Teams <- randomForest(WSWin ~., data = newTeamsTrain1, mtry=5, ntree=500)
rf.predict <- predict(rf.Teams, newdata = newTeamsTest)
table(rf.predict, newTeamsTest$WSWin)
(18+12)/66 
# 0.45 # no improvement over pruned model


varImpPlot(rf.Teams, main = "World Series Prediction Model", color = "blue")

# improve the random forest parameters- tuning
library(caret)
library(e1071)
set.seed(1234)

# Run the model
rf_default <- train(WSWin~.,
                    data =newTeamsTrain1,
                    method = "rf",
                    trControl= trainControl(),
                    metric = "Accuracy")
# Print the results
print(rf_default)
# 13 was best mtry
# let's try anywhere from 1 to 27 as the number of mtry
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1:27))
rf_mtry <- train(WSWin~.,
                 data = newTeamsTrain1,
                 method = "rf",
                 trControl= trainControl(),
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 importance = TRUE,
                 nodesize = 5,
                 ntree = 300)
# takes some time to compute each possibility
warnings()
print(rf_mtry)
# best mtry was 11
# let's get best maxnode
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = 11)
for (maxnodes in c(3:10)) {
  set.seed(1234)
rf_maxnode <- train(WSWin~.,
                      data = newTeamsTrain1,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl= trainControl(),
                      importance = TRUE,
                      nodesize = 5,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
# 8 had highest accuracy

#now let's see for tree numbers
store_maxtrees <- list()
for (ntree in c(200, 250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(WSWin~.,
                       data = newTeamsTrain1,
                       method = "rf",
                       metric = "Accuracy",
                       trControl= trainControl(),
                       tuneGrid = tuneGrid,
                       importance = TRUE,
                       nodesize = 5,
                       maxnodes = 8,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
# best accuracy was 200
# lets plug in those numbers of mtry=11 and max_nodes=8 and ntree=200

set.seed(1234)
rf.TeamsImproved <- randomForest(WSWin ~., data = newTeamsTrain1, mtry=11, ntree=200,
                                 maxnodes= 8)
rf.predictImproved <- predict(rf.TeamsImproved, newdata = newTeamsTest)
table(rf.predictImproved, newTeamsTest$WSWin)
28/66
# 0.424 pretty bad
# no improvement over pruned model

