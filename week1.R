summary(rpois(100, lambda=1))
summary(rpois(100, lambda=100))

var(rpois(100, lambda=100))
var(rpois(100, lambda=1))

set.seed(31);
heightsCM = rnorm(30,mean=188, sd=5);
weightsK = rnorm(30,mean=84,sd=3); 
hasDaughter = sample(c(TRUE,FALSE),size=30,replace=T); 
dataFrame = data.frame(heightsCM,weightsK,hasDaughter); 

# greater than 188 cm tall
dataFrameSubset <- dataFrame[heightsCM > 188, ]
mean(dataFrameSubset$weightsK)

set.seed(41)
cauchyValues <- rcauchy(100)
set.seed(415)
sample(cauchyValues, size=10, replace=TRUE)