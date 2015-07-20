## Practice Questions
wine = read.csv("wine.csv")
str(wine)
PriceModel ~ lm(Price ` WinterRain+ HarvestRain, data=wine)
> summary(PriceModel)

### Climate Change

clm_data = read.csv("climate_change.csv")
# Split data in two parts
clm_train = subset(clm_data,Year<=2006)
clm_test = subset(clm_data,Year>2006)
# Creating a regression model to predict the Temp
clm_model = lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data = clm_train)
summary(clm_model)
clm_model2 = lm(Temp~MEI+N2O+TSI+Aerosols, data = clm_train)
summary(clm_model2)
clm_model3 = step(clm_model)
summary(clm_model3)
clm_predict=predict(clm_model3,clm_test)
SSE = sum((clm_predict - clm_test$Temp)^2)
SST = sum((clm_test$Temp - mean(clm_train$Temp))^2)
1-SSE/SST

######ReadinG Test Scores

pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
nrow(pisaTest)

# Refactor the raceeth variable to contain levels by reference white
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore~. , pisaTrain)
summary(lmScore)
rmse = sqrt(sum(lmScore$residuals^2)/nrow(pisaTrain))
predTest = predict(lmScore,pisaTest)
summary(predTest)

SSE = sum((predTest - pisaTest$readingScore)^2)
SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
1-SSE/SST
RMSE = sqrt(sum((predTest - pisaTest$readingScore)^2)/nrow(pisaTest))


########GooGle Nowcasting

FluTrain = read.csv("FluTrain.csv")
which.max(FluTrain$ILI)
FluTrain$Week[303]
hist(FluTrain$ILI)
plot(log(FluTrain$ILI),FluTrain$Queries)
FluTrend1 = lm(log(ILI)~Queries,FluTrain)
summary(FluTrend1)
FluTrend1 = lm(log(ILI)~Queries,FluTrain)
cor(log(FluTrain$ILI),FluTrain$Queries)
(cor(log(FluTrain$ILI),FluTrain$Queries))^2
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
RMSE = sqrt(sum((PredTest1 - FluTest$ILI)^2)/nrow(FluTest))

# Time Series Model

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))
FluTrend2 = lm(log(ILI)~log(ILILag2)+Queries ,FluTrain)
summary(FluTrend2)


ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
RMSE = sqrt(sum((PredTest2 - FluTest$ILI)^2)/nrow(FluTest))