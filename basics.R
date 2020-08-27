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

########################################################
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )


set.seed(1)
nulls <- vector("numeric", 1000)
for(i in 1:1000){
  random_sample <- sample(x, 50)
  nulls[i] <- mean(random_sample)
}

obs = mean(x)

mean(abs(nulls-obs)>1)
  
########################################################
library(gapminder)
data(gapminder)
head(gapminder)
x=gapminder %>% 
  filter(year==1952) %>% 
  select(country, lifeExp) %>% unlist() %>% 
  as.vector("numeric")

gapminder %>% 
  filter(year==1952) %>% 
  summarise(mean(lifeExp>=40 & lifeExp<=60))

prop = function(q) {
  mean(x <= q)
}

prop(40)

qs = seq(from=min(x), to=max(x), length=20)

props = sapply(qs, prop)

plot(qs, props)
props = sapply(qs, function(q) mean(x <= q))
plot(ecdf(x))
################################################
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}


par(mfrow=c(1,2))
hist(averages5)
hist(averages50)

pnorm(25,23.9, 0.43) - pnorm(23,23.9, 0.43)
################################################
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 

dat <- na.omit( dat )

library(dplyr)
x <- dat %>%
  filter(Sex=='F') %>% 
  filter(Diet=='chow') %>% 
  select(Bodyweight) %>% 
  unlist()

mean(x)

library(rafalib)
popsd(x)

set.seed(2)
X=sample(x, 25)

mean(X)

y=dat %>% 
  filter(Sex=='F') %>% 
  filter(Diet=='hf') %>% 
  select(Bodyweight) %>% 
  unlist()

mean(y)
popsd(y)

set.seed(2)
Y=sample(y, 25)

mean(Y)

diff1=mean(y)-mean(x)
diff2=mean(Y)-mean(X)
abs(diff1-diff2)
