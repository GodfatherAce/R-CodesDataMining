setwd("/Users/RahulAgarwal/Downloads/edx-downloader-master/Downloaded/The Analytics Edge/lecture6")

news = read.csv("dailykos.csv")
str(news)

distance= dist(news, method="euclidean")
hclustmod = hclust(distance,method="ward")
plot(hclustmod)
newsclusters = cutree(hclustmod,k=7)
cluster1 = subset(news,newsclusters==1)
cluster2 = subset(news,newsclusters==2)
cluster3 = subset(news,newsclusters==3)
cluster4 = subset(news,newsclusters==4)
cluster5 = subset(news,newsclusters==5)
cluster6 = subset(news,newsclusters==6)
cluster7 = subset(news,newsclusters==7)


tail(sort(colMeans(cluster1)))

set.seed(1000)
KMC = kmeans(news,centers=7)
str(KMC)
newsclusters = KMC$cluster
cluster1 = subset(news,newsclusters==1)
cluster2 = subset(news,newsclusters==2)
cluster3 = subset(news,newsclusters==3)
cluster4 = subset(news,newsclusters==4)
cluster5 = subset(news,newsclusters==5)
cluster6 = subset(news,newsclusters==6)
cluster7 = subset(news,newsclusters==7)

tail(sort(colMeans(cluster1)))


############
#MARKET SEGMENTATION FOR AIRLINES

############

airlines = read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)

# Normalizing data

library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
distance = dist(airlinesNorm,method= "euclidean")
hcustmod = hclust(distance, method = "ward.D")
plot(hcustmod)
airclusters = cutree(hcustmod, k =5)
table(airclusters)
tapply(airlines$DaysSinceEnroll, airclusters, mean)

set.seed(88)
KMC = kmeans(airlinesNorm,centers = 5, iter.max =1000)
str(KMC)



################
STOCK CLUSTER
CLUSTER THAN PREDICT
################
stocks = read.csv("StocksCluster.csv")
str(stocks)
cor(stocks)


set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel =  glm(PositiveDec~. ,data = stocksTrain,family = binomial)
preds = predict(StocksModel,type = "response")
table(stocksTrain$PositiveDec, preds>=0.5)

preds = predict(StocksModel,type = "response", newdata = stocksTest)
table(stocksTest$PositiveDec, preds>=0.5)


# clustering the stocks data 

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL

limitedTest = stocksTest
limitedTest$PositiveDec = NULL


# Normalizing 

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
summary(normTest$ReturnJan)


set.seed(144)
km = kmeans(normTrain,centers = 3)
str(km)


# What are the cluster that the test data falls in ???
install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)


# Training Models

stocksTrain1 = subset(stocksTrain,clusterTrain==1)
stocksTrain2 = subset(stocksTrain,clusterTrain==2)
stocksTrain3 = subset(stocksTrain,clusterTrain==3)

stocksTest1 = subset(stocksTest,clusterTest==1)
stocksTest2 = subset(stocksTest,clusterTest==2)
stocksTest3 = subset(stocksTest,clusterTest==3)


StocksModel1 =  glm(PositiveDec~. ,data = stocksTrain1,family = binomial)
StocksModel2 =  glm(PositiveDec~. ,data = stocksTrain2,family = binomial)
StocksModel3 =  glm(PositiveDec~. ,data = stocksTrain3,family = binomial)
StocksModel3

PredictTest1 = predict(StocksModel1,type = "response", newdata = stocksTest1)
PredictTest2 = predict(StocksModel2,type = "response", newdata = stocksTest2)
PredictTest3 = predict(StocksModel3,type = "response", newdata = stocksTest3)

table(stocksTest1$PositiveDec, PredictTest1>=0.5)
(30+774)/nrow(stocksTest1)
table(stocksTest2$PositiveDec, PredictTest2>=0.5)
(388+757)/nrow(stocksTest2)
table(stocksTest3$PositiveDec, PredictTest3>=0.5)
(49+13)/nrow(stocksTest3)

# ovrall accuracy

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes,AllPredictions>0.5)
