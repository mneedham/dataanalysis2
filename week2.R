library(kernlab)
data(spam)
dim(spam)

spam[1:10,]

set.seed(3435)
trainIndicator = rbinom(4601, size=1, prob=0.5)
table(trainIndicator)

trainSpam = spam[trainIndicator == 1,]
testSpam = spam[trainIndicator == 0,]

dim(trainSpam)

head(trainSpam)

table(trainSpam$type)

plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

plot(log10(trainSpam[,1:4]+1))

hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

hClusterUpdated = hclust(dist(t(log10(trainSpam[,1:55] +1))))
plot(hClusterUpdated)

trainSpam$numType = as.numeric(trainSpam$type)-1
costFunction = function(x,y) { sum(x!= (y > 0.5)) }
cvError = rep(NA,55)
library(boot)
for(i in 1:55) {
  lmFormula = as.formula(paste("numType~", names(trainSpam)[i], sep=""))
  glmFit = glm(lmFormula, family="binomial", data=trainSpam)
  cvError[i]=cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
which.min(cvError)

names(trainSpam)[which.min(cvError)]

# this doesn't seem to work because the dimensions are different...e
predictionModel = glm(numType ~ charDollar, family="binomial", data=trainSpam)
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])
predictedSpam[predictionModel$fitted > 0.5] = "spam"

table(predictedSpam, testSpam$type)

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./cameras.csv", method="curl")

list.files()

dateDownloaded <- date()
dateDownloaded

?download.file

cameras <- read.table("cameras.csv", sep=",", header=TRUE)
cameras <- read.csv("cameras.csv")
head(cameras)

# interactive way of choosing which file to read
cameras <- read.csv(file.choose())

con <- file("cameras.csv", "r")
cameras <- read.csv(con)
close(con)

con <- url("http://simplystatistics.org", "r")
simplyStats <- readLines(con)
close(con)
head(simplyStats)

# install.packages("RJSONIO", repos = "http://www.omegahat.org/R", type = "source")
install.packages("RSJSONIO")
install.packages("RJSONIO", repos = "http://www.omegahat.org/R", type = "source")

install.packages("XML")
library(XML)

html3 <- htmlTreeParse("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl-en", useInternalNodes=T)
xpathSApply(html3, "//title", xmlValue)

fileUrl <- "http://earthquake.usgs.gov/earthquakes/feed/csv/1.0/week"
download.file(fileUrl, destfile="earthquakeData.csv", method="curl")

eData <- read.csv("earthquakeData.csv")
head(eData)

dim(eData)
names(eData)
nrow(eData)

quantile(eData$Lat)
summary(eData)
class(eData)
sapply(eData[1,], class)

unique(eData$Source)
table(eData$Source)

table(eData$Source, eData$Version)

eData[eData$Latitude > 0 & eData$Longitude > 0, c("Latitude", "Longitude")]

names(cameras) <- tolower(names(cameras))

splitNames <- strsplit(names(cameras), "\\.")

firstElement <- function(x) { x[1] }
sapply(splitNames, firstElement)

fileUrl1 <- "https://dl.dropbox.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropbox.com/u/7710864/data/solutions-apr29.csv"

download.file(fileUrl1, destfile="reviews.csv", method="curl")
download.file(fileUrl2, destfile="solutions.csv", method="curl")
reviews <- read.csv("reviews.csv")
solutions <- read.csv("solutions.csv")

names(reviews)

names(reviews) <- gsub("_", "", names(reviews),)
names(reviews)

reviews$timeleft[1:10]

timeRanges <- cut(reviews$timeleft, seq(0, 3600, by=600))
timeRanges[1:10]
table(timeRanges, useNA="ifany")

install.packages("Hmisc")
library(Hmisc)
timeRanges <- cut2(reviews$timeleft, g=6)
reviews$timeRanges <- timeRanges
head(reviews, 2)

mergedData <- merge(reviews, solutions, all=TRUE)
head(mergedData)

mergedData <- merge(reviews, solutions, by.x="solutionid", by.y="id", all=TRUE)

head(mergedData)

# Quiz

con <- url("http://simplystatistics.tumblr.com/", "r")
simplyStats <- readLines(con)
close(con)
numberChars <- sapply(simplyStats, nchar)
numberChars[c(2,45,122)]

head(numberChars)

file <- "https://dl.dropbox.com/u/7710864/data/csv_hid/ss06hid.csv"
download.file(file, destfile="americancommunity.csv", method="curl")

acomm <- read.csv("americancommunity.csv")
head(acomm)
names(acomm)

length(acomm$VAL[acomm$VAL == 24])

greaterMillion <- acomm$VAL == 24
length(Filter(function(x) x== TRUE, greaterMillion))

head(acomm$FES)


one <- acomm[acomm$RMS == 4 & !is.na(acomm$RMS) & acomm$BDS == 3 & !is.na(acomm$BDS), c("RMS")]
two <- acomm[acomm$RMS == 5 & !is.na(acomm$RMS) & acomm$BDS == 2 & !is.na(acomm$BDS), c("RMS")]
three <- acomm[acomm$RMS == 7 & !is.na(acomm$RMS) & acomm$BDS == 2 & !is.na(acomm$BDS), c("RMS")]

length(one)
length(two)
length(three)

lapply(c(one, two, three), length)



agricultureLogical <- acomm$AGS == 6 & !is.na(acomm$AGS) & acomm$ACR==3 & !is.na(acomm$ACR)
agricultureLogical <- acomm$AGS == 6 & acomm$ACR==3 
indexes <- which(agricultureLogical)

which(agricultureLogical)

subsetDataFrame <- acomm[indexes,]

length(Filter(function(x) x == TRUE, is.na(subsetDataFrame$MRGX)))

names(acomm)
strsplit(names(acomm), "wgtp")[123]

summary(acomm$YBL, na.rm=FALSE)

acomm$YBL

file <- "https://dl.dropbox.com/u/7710864/data/csv_hid/ss06pid.csv"
download.file(file, destfile="pop.csv", method="curl")
populationData <- read.csv("pop.csv")

housingData <- read.csv("americancommunity.csv")

merged <- merge(housingData, populationData, by.x="SERIALNO", by.y="SERIALNO", all=TRUE)

dim(merged)

