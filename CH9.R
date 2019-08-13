library(SixSigma)
library(tidyverse)
library(ggplot2)
library(qcc)

##### X-bar Chart #####
ss.data.thickness2 %>% 
  aggregate(thickness ~ ushift,
            data = ., 
            FUN = mean) # 以ushift進行分組，計算平均

# 以qcc.group對數據進行分組
samples.thick <- qcc.groups(ss.data.thickness2$thickness, ss.data.thickness2$ushift) %>% view()

range_bar <- samples.thick %>% 
  apply(X = ., MARGIN = 1, FUN = range) %>% 
  apply(X = ., MARGIN = 2, FUN = diff) %>% 
  mean()

##### Creat the qcc #####
xbar.thick <- qcc(data = samples.thick, type = "xbar")
summary(xbar.thick)
plot(xbar.thick)
names(xbar.thick)

# 驗證 ？？
range_bar <- samples.thick %>% 
  apply(X = ., MARGIN = 1, FUN = range) %>% 
  apply(X = ., MARGIN = 2, FUN = diff) %>% 
  mean()

xbar.thick$center+range_bar*(3/ 2.534413*(6^0.5))

xbar.thick$nsigmas
xbar.thick$violations

qcc.options("beyond.limits" = list(pch = 20, col = "red3")) # beyond.limits 調整離群值樣式 
qcc.options(bg.margin = "azure2")
plot(xbar.thick,
     axes.las = 1, # 調整 x 軸表示法
     digits = 3, # 顯示到小數第幾位
     title = "X-Bar chart metal plates thickness",
     xlab = "shift",
     ylab = "Sample mean",
     ylim = c(0.7, 0.8)) # Y軸範圍

thick.betas <- oc.curves(xbar.thick) # 裡面的值代表 Beta
1 / (1- thick.betas[rownames(thick.betas) == "1", 1]) # 平均3.43個產品會有一個瑕疵


##### Range Chart #####
r.thick <- qcc(data = samples.thick, type = "R") # Still in Control

##### SD Chart #####
s.thick <- qcc(data = samples.thick, type = "S")

##### X- bar & S Chart #####
set.seed(1)
new.sample <- matrix(round(rnorm(6, 0.75, 0.05), 3),
                     nrow = 1, ncol = 6)
mean(new.sample)

ccxbar <- qcc(data = samples.thick, 
              type = "xbar",
              newdata = new.sample, 
              newlabels = "8.1") # newdata 在X軸的標示
ccs <- qcc(data = samples.thick, 
           type = "S",
           newdata = new.sample, 
           newlabels = "8.1")
par(mfrow = c(2 , 1))
plot(ccxbar, restore.par = F, add.stats = F)
plot(ccs, add.stats = F)


##### I & MR control charts #####
thickness2days <- ss.data.thickness2$thickness[1 : 24]
mov.samples <- cbind(thickness2days[1 : 23], 
                     thickness2days[2 : 24])
par(mfrow = c(1 , 1))
cci <- qcc(thickness2days, type = "xbar.one")
ccmr <- qcc(mov.samples, type = "R")
par(mfrow = c(2, 1))
plot(cci, restore.par = F, add.stats = F)
plot(ccmr, add.stats = F)

##### p and np chart #####
# the vector with the proportions for each smaple
thick.attribute <- aggregate(thickness ~ ushift, 
                             data = ss.data.thickness2,
                             FUN = function (x){
                               sum(x > 0.775)
                             }) 

# p chart 
par(mfrow = c(1 , 1))
thick.p <- qcc(data = thick.attribute$thickness,
               type = "p",
               size = 6)
# np chart 
thick.np <- qcc(data = thick.attribute$thickness,
                type = "np",
                size = 6)

par(mfrow = c(2, 1))
plot(thick.p, restore.par = F, add.stats = F)
plot(thick.np, add.stats = F)

##### c and u chart #####
# 要先去掉 NA值 
flaws <- ss.data.thickness2$flaws [ ! is.na (ss.data.thickness2$flaws) ]

# c chart 
par(mfrow = c(1 , 1))
thick.c <- qcc(data = flaws, type = "c")

# u chart 
# 計算同一個群組裡, flaws 加起來總共有多少
shift.flaws <- aggregate(flaws ~ ushift,
                         data = ss.data.thickness2,
                         FUN = sum,
                         na.rm = T) [, 2]

# 計算同一個群組裡面 有多少個是沒有NA的
shift.inspected <- aggregate(flaws ~ ushift,
                             data = ss.data.thickness2,
                             FUN = function(x){
                               sum(!is.na(x))
                             }) [, 2] 

thick.u <- qcc(data = shift.flaws,
               type = "u",
               size = shift.inspected) 

names(thick.u)
thick.u$limits
thick.u$center

par(mfrow = c(2, 1))
plot(thick.c, restore.par = F, add.stats = F)
plot(thick.u, add.stats = F)
