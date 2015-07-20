setwd("/Users/RahulAgarwal/Downloads/edx-downloader-master/Downloaded/The\ Analytics\ Edge/lecture4")

gerber = read.csv("gerber.csv")

str(gerber)

sum(gerber$voting)/nrow(gerber)

# creting a logistic regreession

logmodel = glm(voting~hawthorne+civicduty+self+neighbors,data=gerber,family = binomial)

preds = predict(logmodel,data = gerber,type = "response")

library(ROCR)
rocrpred = prediction(preds,gerber$voting)
auc = as.numeric(performance(rocrpred,"auc")@y.values)



###trees

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
library(rpart.plot)
prp(CARTmodel)


letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
preds = predict(CARTb,newdata=test,type = "class")
table(test$isB,preds)

library(randomForest)
set.seed(1000)
fitRF = randomForest(isB ~ . - letter, data = train)
preds = predict(fitRF,newdata=test,type = "class")
table(test$isB,preds)


letters$letter = as.factor( letters$letter ) 

set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)

CARTb = rpart(letter ~ . - isB, data=train, method="class")
preds = predict(CARTb,newdata=test,type = "class")
table(test$letter,preds)

set.seed(1000)
fitRF = randomForest(letter ~ . - isB, data = train)
preds = predict(fitRF,newdata=test,type = "class")
table(test$letter,preds)


#census


census = read.csv("census.csv")

library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split == TRUE)
test = subset(census, split == FALSE)



logmodel = glm(over50k~.,data=train,family = binomial)
preds = predict(logmodel,newdata = test,type = "response")
table(test$over50k,preds>0.5)

library(ROCR)
rocrpred = prediction(preds,test$over50k)
auc = as.numeric(performance(rocrpred,"auc")@y.values)

CARTb = rpart(over50k ~ . , data=train, method="class")
preds = predict(CARTb,newdata=test)
rocrpred = prediction(preds[,2],test$over50k)
auc = as.numeric(performance(rocrpred,"auc")@y.values)


set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
fitRF = randomForest(over50k ~ . , data = trainSmall)
preds = predict(fitRF,newdata=test,type = "class")
table(test$over50k,preds)

# variable importance based on how many times its used to split
vu = varUsed(fitRF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(fitRF$forest$xlevels[vusorted$ix]))
# Impurity
varImpPlot(fitRF)


cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 

library(caret)
library(e1071)
tr.control= trainControl(method="cv",number =10)
set.seed(2)
treeM  = train(over50k~.,data=train,method="rpart",trControl = tr.control,tuneGrid = cartGrid)
treeM

CARTb = rpart(over50k ~ . , data=train, method="class",cp = 0.002)
preds = predict(CARTb,newdata=test,type = "class")
table(test$over50k,preds)


