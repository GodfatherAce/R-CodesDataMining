setwd("/Users/RahulAgarwal/Downloads/edx-downloader-master/Downloaded/The\ Analytics\ Edge/lecture3")

#### Predicting Top Ten
songs = read.csv("songs.csv")
#Songs in 2010
sum(songs$year==2010)
#Highest tempo song
songs$songtitle[which.max(songs$tempo)]
#Creating a Test And Training set
SongsTrain = subset(songs,year<=2009)
SongsTest = subset(songs,year>2009)
#Creating the model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
# Seems like energy and loudness are collinear
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
# Preddict on est set using model 3
predictTest = predict(SongsLog3,newdata = SongsTest,type ="response")
table(SongsTest$Top10,predictTest>=0.45)


#### Predicting Parole Violators

parole = read.csv("parole.csv")
#create factor variables
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
ParoleLog1 = glm(violator ~ . , data=train, family=binomial)
odds =exp(-4.2411574 + 0.3869904 + 0.8867192 + -0.0001756*50 + -0.1238867*3 +0.0802954*12+0.6837143)
preds= predict(ParoleLog1,newdata = test, type = "response")
table(test$violator,preds>=0.5)
library(ROCR)
ROCRpred = prediction(preds,test$violator)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)


#####Predicting the defaulters

loans = read.csv("loans.csv")
#impute the data
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

#Split in training and test
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
LoansLog1 = glm(not.fully.paid ~ . , data=train, family=binomial)

test$predicted.risk = predict(LoansLog1,newdata = test, type = "response")
table(test$violator,test$predicted.risk>=0.5)
library(ROCR)
ROCRpred = prediction(test$predicted.risk,test$not.fully.paid)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)

#bivariatemodel 
LoansLog2 = glm(not.fully.paid ~ int.rate , data=train, family=binomial)
test$predicted.risk.2 = predict(LoansLog2,newdata = test, type = "response")
table(test$not.fully.paid,test$predicted.risk.2>=0.5)
library(ROCR)
ROCRpred = prediction(test$predicted.risk.2,test$not.fully.paid)
auc = as.numeric(performance(ROCRpred,"auc")@y.values)