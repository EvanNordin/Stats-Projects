require(leaps)
library(MASS)

#training dataset contains obs from 1920-2019
nfl <- read.csv("nfl_elo.csv")

homeTeamWins <- ifelse(nfl$score1 == nfl$score2,NA,ifelse(nfl$score1 > nfl$score2, 1,0))
awayTeamWins <- ifelse(nfl$score1 == nfl$score2,NA,ifelse(nfl$score2 > nfl$score1, 1,0))
nfl$homeTeamWins = homeTeamWins #Creating a new variable in the dataset for the created indicators
nfl$awayTeamWins = awayTeamWins #Creating a new variable in the dataset for the created indicators

par(mfrow=c(4,1))

plot(nfl$homeTeamWins~nfl$elo1_pre)
plot(nfl$awayTeamWins~nfl$elo2_pre)
plot(nfl$homeTeamWins~nfl$qbelo1_pre)
plot(nfl$awayTeamWins~nfl$qbelo2_pre)
plot(nfl$homeTeamWins~nfl$qbelo_prob2)
plot(nfl$awayTeamWins~nfl$qb1_value_pre)
plot(nfl$homeTeamWins~nfl$qb2_value_pre)
plot(nfl$awayTeamWins~nfl$qb1_adj)
plot(nfl$awayTeamWins~nfl$qb2_adj)


plot(nfl$homeTeamWins~nfl$qbelo1_pre, xlim=c(1100,1800),xlab="ELO Probability 1", ylab="Home Team Won", main="Correlation between home team wins and ELO probabilities")
plot(nfl$awayTeamWins~nfl$qbelo2_pre, xlim=c(1100,1800),xlab="ELO Probability 2", ylab="Away Team Won", main="Correlation between away team wins and ELO probabilities")

#test data set contains obs from 2020/2021 NFL season
sb <- read.csv("nfl_elo_latest.csv")
homeTeamWins <- ifelse(sb$score1 == sb$score2,NA,ifelse(sb$score1 > sb$score2, 1,0))
awayTeamWins <- ifelse(sb$score1 == sb$score2,NA,ifelse(sb$score2 > sb$score1, 1,0))
sb$homeTeamWins = homeTeamWins #Creating a new variable in the dataset for the created indicators
sb$awayTeamWins = awayTeamWins

#Using logarithmic regression with the 9 predictors we found to be best
logModel <- glm(homeTeamWins~elo1_pre+elo2_pre+qbelo2_pre+qbelo1_pre+qbelo_prob2+qb1_value_pre+qb2_value_pre+qb1_adj+qb2_adj,family=binomial(link='logit'), data= nfl)
plot(logModel)
summary(logModel)

#using linear regression with same 9 predictors to try and predict scores
linMod <- lm(score1~elo1_pre+elo2_pre+qbelo2_pre+qbelo1_pre+qbelo_prob2+qb1_value_pre+qb2_value_pre+qb1_adj+qb2_adj,data= nfl)
plot(linMod)
summary(linMod)
predict(linMod, data.frame(sb))

MSE<-mean((linMod$residuals)^2)
RMSE<-sqrt(MSE)                

#using the linear regression we just created to try and predict who wins
logPredict<-predict(logModel, data.frame(sb))


threshold <- ifelse(logPredict > .05,1,0)
table(logPredict)
table(threshold, sb$homeTeamWins, main = "Predictions VS Actual Values")


#sensitivity for home team winning is predicted/actual
107/133 # 80.5%

#specificity for home team losing is predicted/actual
77/134 # 57.5%

#Correctly predicted that home team (team 1 wins)


#LDA just for fun

#Trying linear discriminant analysis to predict outcome of superbowl
sb.lda <- lda(homeTeamWins~elo1_pre+elo2_pre+elo_prob1+elo_prob2+qb1_value_pre+qb2_value_pre+qb1_adj+qb2_adj+qbelo_prob1+qbelo_prob2, data = sb)
plot(sb.lda)
lda.prediction <- predict(sb.lda, sb)
lda.prediction

threshold <- ifelse(lda.prediction$posterior[,2] > .05,1,0)
table(lda.prediction$class)
table(lda.prediction$class,sb$homeTeamWins)   #specificity = 87/134 = 64.9% and sensistivity = 94/133 = 70.7%
mean(lda.prediction$class == sb$homeTeamWins)

#Trying logistic regression (doesn't look too valuable for predicting superbowl since only one predictor out the available predictor variables is significant)
logModel = glm(sb$homeTeamWins~sb$elo1_pre+sb$elo2_pre+sb$elo_prob1+sb$elo_prob2+sb$qb1_value_pre+sb$qb2_value_pre+sb$qb1_adj+sb$qb2_adj+sb$qbelo_prob1+sb$qbelo_prob2, family=binomial(link='logit'))

summary(logModel)
plot(regModel)


Pred1 <- subset(sb, team1 == "KC" | team2 == "KC")
sbPred2 <- subset(sb, team2 == "TB" | team2 == "TB") 

pred1TS <- ts(sbPred1)
ts.plot(pred1TS)
