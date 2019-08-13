#Simple Random Sampling
library(SixSigma)
str(ss.data.bills)
# 32 observation, 3 var.
# nbill：bill identification
# clerk：clerk name
# errors：count of errors in the bill
View(ss.data.bills)

#Q：Thirty-two bills were produced yesterday, and the supervisor wishes to check eight of them in detail.
set.seed(29)
billsRandom <- sample(ss.data.bills$nbill,  #vector
                      size = 8,             #sample size
                      replace = FALSE)      #不重複抽樣

billsSample <- subset(ss.data.bills,
                      nbill %in% billsRandom)

mean(billsSample$errors)
# ==> 1


#Stratified Sampling
## Population proportion
table(ss.data.bills$clerk)/length(ss.data.bills$clerk)
# ==> John：0.75；Mary：0.25

## Simple sample proportion
table(billsSample$clerk)/length(billsSample$clerk)
# ==> John：0.625；Mary：0.375

billsMary <- ss.data.bills$nbill[ss.data.bills$clerk == "Mary"]

billsJohn <- ss.data.bills$nbill[ss.data.bills$clerk == "John"]

set.seed(29)
billsRandomMary <- sample(billsMary, 2)
billsRandomJohn <- sample(billsJohn, 6)
billsRandomStrat <- c(billsRandomMary, billsRandomJohn)

billsSampleStrat <- subset(ss.data.bills,
                           nbill %in% billsRandomStrat)

eSampleMary <- subset(billsSampleStrat,
                      clerk == "Mary",
                      errors,
                      drop = TRUE) #參數設定不保留data.frame狀態
is.data.frame(eSampleMary) # ==> FALSE
is.vector(eSampleMary) # ==> TRUE
is.data.frame(subset(billsSampleStrat,
                     clerk == "Mary",
                     errors,
                     drop = FALSE)) # ==> TRUE

eSampleJohn <- subset(billsSampleStrat,
                      clerk == "John",
                      errors,
                      drop = TRUE)

weighted.mean(x = c(mean(eSampleMary),mean(eSampleJohn)),
              w = c(0.25, 0.75))
# ==> 0.625
mean(ss.data.bills$errors)
# ==>0.71875


#OC curve
pdensity <- c(10.6817, 10.6040, 10.5709, 10.7858,
              10.7668, 10.8101, 10.6905, 10.6079,
              10.5724, 10.7736, 11.0921, 11.1023,
              11.0934, 10.8530, 10.6774, 10.6712,
              10.6935, 10.5669, 10.8002, 10.7607,
              10.5470, 10.5555, 10.5705, 10.7723)
##every four measurements make up a group
gdensity <- rep(1:6, each = 4)
library(qcc)
myGroups <- qcc.groups(data = pdensity,
                       sample = gdensity)

myqcc <- qcc(myGroups, type = "xbar", plot = FALSE)
mybeta <- oc.curves(myqcc)
head(mybeta)

#detect a 1.5 standard deviations depart from the mean
mybeta["1.5",]
# ==> n=10, beta=0.0406304449, power>=95%