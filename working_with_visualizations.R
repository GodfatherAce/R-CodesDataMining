setwd("/Users/RahulAgarwal/Downloads/edx-downloader-master/Downloaded/The Analytics Edge/lecture7")
install.packages("maps")
install.packages("ggmap")

# MAPS
library(maps)
library(ggmap)

statesMap = map_data("state")
str(statesMap)
table(statesMap$group)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 
polling = read.csv("PollingImputed.csv")
str(polling)
Train = subset(polling,Year==2004 |Year==2008)
Test = subset(polling,Year==2012) 
str(test)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",alpha=.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
?geom_polygon


## GRAPHS

edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(users)
install.packages("igraph")
library(igraph)
g = graph.data.frame(edges, FALSE, users) 
plot(g, vertex.size=5, vertex.label=NA)
sum(degree(g)>=10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)


## WORD CLOUDS

# Read in the data

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)
tweets$Avg<=-1
# Install new packages

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus[[1]]

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)


corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

install.packages("wordcloud")
library(wordcloud)
?wordcloud

wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.25))


alltweets_negatives = subset(allTweets,tweets$Avg<=-1)
wordcloud(colnames(alltweets_negatives),colSums(alltweets_negatives),scale=c(2, 0.25))


install.packages("RColorBrewer")
library(RColorBrewer)
brewer.pal()
display.brewer.all()
