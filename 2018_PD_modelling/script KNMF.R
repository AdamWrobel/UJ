rm(list=ls())

#optional - install packages
# install.packages("dplyr")
# install.packages("ROCR")

#load libraries
library(dplyr)
library(ROCR)

#set working directory - please select the folder where you saved script and data files
setwd("C:/UBS/Dev/Students/Working folder/")

############## DATA CHECK #####################

# C1: read the data
Data <- read.delim("DataPD.txt", stringsAsFactors = F)
CriteriaSummary <- read.delim("Description.txt", stringsAsFactors = F)

# C2: set useful variables
VarNames <- CriteriaSummary[, "Criteria"]

# C3: look into the data
View(Data)
# or
head(Data)
#what can we observe?

# and the data description - criteria have economic interpretation, grouped into four categories
CriteriaSummary

# C4 Check histograms

plot(density(Data[, VarNames[1]], na.rm = T), main = VarNames[1])

#the same command but in pipeline notation - from dplyr package
Data[, VarNames[1]] %>% density(na.rm = T) %>% plot(main = VarNames[1])

#maybe several histograms in one picture
par(mfrow = c(3,4))
for(i in 1:length(VarNames)){
  Data[, VarNames[i]] %>% density(na.rm = T) %>% plot(main = VarNames[i])
}

#what do we observe? is that extreme data concentration?

# C5 check variable distribution parameters
quantile(Data[, VarNames[1]], seq(0, 1, by = 0.1), na.rm = T)
#what do we conclude? what about maximums?
DataQuantiles <- matrix(nrow = length(VarNames), ncol = 12)
#anyone eager to write a loop summarizing quantiles for all variables in a matrix?

# C6 remove outliers - an idea to obtain reasonable histograms
par(mfrow = c(3,4))
for(i in 1:length(VarNames)){
  LowerBound <- quantile(Data[, VarNames[i]], 0.05, na.rm = T)
  UpperBound <- quantile(Data[, VarNames[i]], 0.95, na.rm = T)
  Data[Data[, VarNames[i]] >= LowerBound & Data[, VarNames[i]] <= UpperBound , VarNames[i]] %>% density(na.rm = T) %>% plot(main = VarNames[i])
}

######################## PROBIT MODEL ###############


# C7 Check AUC for variables 
Pred <- prediction(Data[,VarNames[1]],Data[,"deflag"])
AUC <- as.numeric(performance(Pred,"auc")@y.values)
#plot ROC curve
plot(performance(Pred, "tpr", "fpr"))
lines(seq(0,1,by = 0.01), seq(0,1,by = 0.01), col = "red")

# TASK: anyone eager to write a loop summarizing AUC in one matrix (or vector using names(vector))?



#there is another way to do so - sapply command
AUCsummary <- sapply(VarNames, function(x) as.numeric(performance(prediction(Data[,x],Data[,"deflag"]),"auc")@y.values))

# what is the data coverage for particular variables?

# TASK: use command sapply to summarize data coverage (proportion of complete observations to all observations)

DataCoverage <- sapply(VarNames, function(x) ...........)

#what is the default coverage for particular varialbes?

DefaultCoverage <- sapply(VarNames, function(x)  sum(Data[, "deflag"] * !is.na(Data[, x])) / sum(Data[, "deflag"]) )

#let's gather everything in one data frame

VariablesSummary <- data.frame( VarNames, AUCsummary, DataCoverage, DefaultCoverage)
#order data. what is the logic below?
VariablesSummary <- VariablesSummary[order(VariablesSummary$AUCsummary, decreasing = T),]

# C8 First probit regressions - three best variables with respect to AUC
Regress <- glm(formula= "deflag ~ var5_LF + var14_LF + var26_LF",na.action=na.exclude,family=binomial("probit"), data = Data)
summary(Regress)
#what is null and residual deviance, AIC? are large or small values desired?

#different approach to Regression coding
CriteriaCodes <- VariablesSummary[1:3, "VarNames"]
Formula <- as.formula(paste("deflag ~ ", gsub(", "," + ",toString(CriteriaCodes))))# The regression formula
Regress <- glm(formula=Formula,na.action=na.exclude,family=binomial("probit"), data = Data)
Regress$coefficients
summary(Regress)

#check of AUC - one step more than in case of univariate analysis - we must produce model score for each observation
FittedScores <- predict.glm(Regress)
Pred <- prediction(FittedScores,Data$deflag)
AUC <- as.numeric(performance(Pred,"auc")@y.values)
#plot ROC curve
plot(performance(Pred, "tpr", "fpr"))
lines(seq(0,1,by = 0.01), seq(0,1,by = 0.01), col = "red")

# C9 take eight best variables

CriteriaCodes <- VariablesSummary[1:8, "VarNames"]
Formula <- as.formula(paste("deflag ~ ", gsub(", "," + ",toString(CriteriaCodes))))# The regression formula
Regress <- glm(formula=Formula,na.action=na.exclude,family=binomial("probit"), data = Data)
Regress$coefficients
summary(Regress)

#check of AUC
FittedScores <- predict.glm(Regress)
Pred <- prediction(FittedScores,Data$deflag)
AUC <- as.numeric(performance(Pred,"auc")@y.values)

#can we increase number of variables ad infinitum? apart from obvious reasons (diffcult interpretation etc) there is an another trap. 
#let's assume that future internal portfolio has the same ccomposition as estimation dataset (in terms of na's)

# C10 data coverage
DataCoverageBigModel <-  nrow(na.omit(Data[,CriteriaCodes]))/nrow(Data) 
DefaultCoverageBigModel <-  sum(Data[, "deflag"] *  !is.na(rowSums(Data[, CriteriaCodes]) )) / sum(Data[, "deflag"])

# TASK: figure out how below command works

#we susbtantially decreased our estimation dataset!

# let's reduce our set of variables

# C11 reduced model 
CriteriaCodes <- c("var5_LF", "var14_LF", "var1_AQ", "var18_CL")
Formula <- as.formula(paste("deflag ~ ", gsub(", "," + ",toString(CriteriaCodes))))# The regression formula
Regress <- glm(formula=Formula,na.action=na.exclude,family=binomial("probit"), data = Data)
Regress$coefficients
summary(Regress)

#check of AUC
FittedScores <- predict.glm(Regress)
Pred <- prediction(FittedScores,Data$deflag)
AUC <- as.numeric(performance(Pred,"auc")@y.values)

DataCoverageBigModel2 <-  nrow(na.omit(Data[,CriteriaCodes]))/nrow(Data) 
DefaultCoverageBigModel2 <-  sum(Data[, "deflag"] *  !is.na(rowSums(Data[, CriteriaCodes]) )) / sum(Data[, "deflag"])


#how this will work when we remove outliers?

# C12 outliers removal

NewData <- Data

for(i in CriteriaCodes){
  print(i)
  LowerBound <- quantile(NewData[, i], 0.05, na.rm = T)
  UpperBound <- quantile(NewData[, i], 0.95, na.rm = T)
  NewData[(NewData[, i] < LowerBound | NewData[, i] > UpperBound) & !is.na(NewData[, i]) , i] <- NA
}

#compare histograms
par(mfrow = c(3,4))
for(i in CriteriaCodes){
  Data[, i] %>% density(na.rm = T) %>% plot(main = i)
  NewData[, i] %>% density(na.rm = T) %>% plot(main = i)
}

#check of coverage

NewDataCoverageBigModel <-  nrow(na.omit(NewData[,CriteriaCodes]))/nrow(NewData) 
NewDataDefaultCoverageBigModel <-  sum(NewData[, "deflag"] *  !is.na(rowSums(NewData[, CriteriaCodes]) )) / sum(NewData[, "deflag"])


#estimate our model on variables without outliers

NewRegress <- glm(formula=Formula,na.action=na.exclude,family=binomial("probit"), data = NewData)
NewRegress$coefficients
summary(NewRegress)

NewFittedScores <- predict.glm(NewRegress)
NewPred <- prediction(NewFittedScores,NewData$deflag)
NewAUC <- as.numeric(performance(NewPred,"auc")@y.values)

#better. How does it stick to full dataset?

#C13 model performance on unchanged dataset

NewFittedScores2 <- predict.glm(NewRegress, newdata = Data)
NewPred2 <- prediction(NewFittedScores2,Data$deflag)
NewAUC2 <- as.numeric(performance(NewPred2,"auc")@y.values)

#seems to be not really helpful..
# idea to impove above - let limits for outlier in "production"
# C14 setting limits

NewData2 <- Data

for(i in CriteriaCodes){
  print(i)
  LowerBound <- quantile(NewData2[, i], 0.05, na.rm = T)
  UpperBound <- quantile(NewData2[, i], 0.95, na.rm = T)
  NewData2[NewData2[, i] < LowerBound  & !is.na(NewData2[, i]) , i] <-  LowerBound
  NewData2[NewData2[, i] > UpperBound  & !is.na(NewData2[, i]) , i] <- UpperBound 
}


NewFittedScores3 <- predict.glm(NewRegress, newdata = NewData2)
NewPred3 <- prediction(NewFittedScores3,NewData2$deflag)
NewAUC3 <- as.numeric(performance(NewPred3,"auc")@y.values)

#better performance

