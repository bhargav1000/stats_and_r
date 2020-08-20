load("skew.RData")

dim(dat)

par(mfrow = c(3,3))

for (i in 1:9) {
  qqnorm(dat[,i])
}
########################################################

par(mfrow=c(1,1))
hist(exec.pay)

qqnorm(exec.pay)

qqline(exec.pay)
########################################################
head(InsectSprays)

boxplot(split(InsectSprays$count, 2))

boxplot(InsectSprays$count ~ InsectSprays$spray)
########################################################
library(dplyr)
data(nym.2002, package="UsingR")
par(mfrow=c(2,2))
for(i in c("Male", "Female")){
  # hist(nym.2002[nym.2002$gender==i,]$time, main=i, xlab="time")  
  # qqnorm(nym.2002[nym.2002$gender==i,]$time, main = i)
  boxplot(nym.2002$gender==i ~ nym.2002$time)
}
########################################################
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
set.seed(5)
abs(mean(sample(x, 5)) - mean(x))
