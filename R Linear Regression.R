#### Using R

setwd("/Users/RahulAgarwal/Downloads/edx-downloader-master/Downloaded/The Analytics Edge/lecture1/")
getwd()
WHO=read.csv("WHO.csv")
str(WHO)
mean(WHO$Over60)
WHO$Country[which.min(WHO$Over60)]
WHO$Country[which.max(WHO$LiteracyRate)]
tapply(WHO$ChildMortality,WHO$Region,mean,na.rm=T)


##### The Analytical Detective:

##### Dataset: mvtWeek1
mvt=read.csv("mvtWeek1.csv")
str(MV)

#No of rows:
nrow(mvt)

# Datetime conversion
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert


# Month and day of least thefts
table(mvt$Month)
table(mvt$Weekday)

# Month with highest arrests
table(mvt$Month,mvt$Arrest)

# plots
hist(mvt$Date, breaks=100)
boxplot(mvt$Date~mvt$Arrest)


sort(table(mvt$LocationDescription))

Top5=subset(mvt,LocationDescription=="STREET"|LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)"|LocationDescription=="ALLEY"|LocationDescription=="GAS STATION"|LocationDescription=="DRIVEWAY - RESIDENTIAL")

Top5$LocationDescription = factor(Top5$LocationDescription)


##### The Stock Dynamics

IBM = read.csv("IBMStock.csv")
GE= read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv") 
CocaCola = read.csv("CocaColaStock.csv") 
Boeing = read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

plot(CocaCola$Date,CocaCola$StockPrice,type="l")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432],col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432],col="black")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],col="orange")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

tapply(GE$StockPrice,months(GE$Date),mean)



#####CPS Data

CPS=read.csv("CPSData.csv")
sort(table(CPS$State)) 
table(CPS$Citizenship)

# why a variable is missing
table(CPS$Region, is.na(CPS$MetroAreaCode))

tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)

# Mapping a dictionary

MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

CPS= merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
CPS= merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
sort(tapply(CPS$Race == "Asian",CPS$MetroArea,mean))