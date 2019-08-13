##### CH5.1 #####
#histogram
day1 <- c(0.821, 0.846, 0.892, 0.750, 0.773, 0.786,
          0.956, 0.840, 0.913, 0.737, 0.793, 0.872)
day2 <- c(0.678, 0.742, 0.684, 0.766, 0.721, 0.785,
          0.759, 0.708, 0.789, 0.732, 0.804, 0.758)
plates <- data.frame(thickness = c(day1, day2),
                     day = rep(c("Day1", "Day2"), each = 12))

hist(plates$thickness,
     main = "Histogram of Thickness",
     xlab = "Thickness (in)",
     las = 1,
     col = gray(0.5),
     border = "white")

# main: plot title
# xlab: label for the x axis
# las: axis labels orientation
# col: histogram bars fill color
# border: histogram bars border color


library(lattice)
trellis.par.set(canonical.theme(color = FALSE))  #false黑白呈現
trellis.par.set("background", list(col = gray(0.85)))  #灰色底
trellis.par.set("panel.background", list(col = "white"))  #panel呈現白底
histogram(~ thickness | day, data = plates,
          main = "Histogram of Thickness by day",
          xlab = "Thickness (in)",
          ylab = "Frequency",
          type = "count")



#run chart
plot(plates$thickness,
     type = "b",
     main = "Run Chart of Thickness",
     las = 1,
     ylab = "Thickness",
     xlab = "Plate number",
     pch = 20)
abline(h = median(plates$thickness),
       lwd = 2)

# type: the type of representation for each data point (lines, points, both)
# pch: the symbol to be plotted at each point
# h: the value in the vertical axis at which a horizontal line will be plotted
# lwd: the line width


#qicharts will no longer be maintained. Please consider moving to qicharts2: https://anhoej.github.io/qicharts2/.
library(qicharts2)
qic(thickness,
    data = plates,
    freeze = 12,  #X=12時將資料分為2個subsets
    part.labels = c("Day 1", "Day2"),
    print.summary = TRUE)

# freeze: Point in the x axis that divides the data set in two subsets
# part.labels: Text to annotate the freeze period
# print.summary: print the statistics from runs analysis



#Tier chart
plates$shift <- factor(paste0("Shift",
                              (rep(1:2, each = 6))))
plates$dayshift <- factor(paste(plates$day,
                                plates$shift, sep = "."))
#paste 預設會使用一個空白字元當作分隔符號，將所有的字串連接起來
#如果不想要有任何分隔符號，可以使用 paste0 這個函數：

library(lattice)
dotplot(thickness ~ dayshift,
        data = plates,
        pch = "-",
        cex = 4,
        panel = function(x, y, ...){
          panel.dotplot(x, y, ...)
          panel.superpose(x, y,
                          subscripts = 1:length(x), x,
                          panel.groups = "llines",
                          col = "black",
                          type = "l",
                          lty = 1)
        })

# cex: Magnifying factor for text and symbols
# panel.superpose: Function to plot different elements by groups within a panel
# subscripts: indices of the data in the original data source to be used
# panel.groups: the name of the function to be used when superposing(疊置)
# lty: Type of line to be plotted (0:blank, 1:solid, 2:dashed, 3:dotted, 4:dotdash, 5:longdash, 6:twodash)



#Box-and-Whisker Plot
boxplot(plates$thickness)
#不同天
boxplot(thickness ~ day, data = plates,
        xlab = "Day",
        ylab = "Thickness (in)")

#不同天不同批
library(lattice)
bwplot(thickness ~ shift | day , data = plates)



# Cetral Tendency
# 1.mean
mean(plates$thickness)
tapply(plates$thickness, plates$day, mean)
# 2.median
median(plates$thickness)
tapply(plates$thickness, plates$dayshift, median)
# 3.mode
thickness.freq <- table(cut(plates$thickness,
                            round(sqrt(length(plates$thickness)))))
thickness.freq
thickness.mode.int <- names(thickness.freq)[thickness.freq == max(thickness.freq)]
thickness.mode.int
table(plates$thickness)

thickness.hist <- hist(plates$thickness, plot = FALSE)
names(thickness.hist)
thickness.mode.mid <- thickness.hist$mids[
  thickness.hist$counts == max(thickness.hist$counts)]
thickness.mode.mid



thickness.central <- list(mean = mean(plates$thickness),
                          median = median(plates$thickness),
                          mode = thickness.mode.mid)
plot(thickness.hist)
abline(v = thickness.central,
       col = c("red4", "green4", "steelblue"),
       lwd = 2)
legend(x = 0.85, y = 8,
       legend = names(thickness.central),
       lwd = 2,
       col = c("red4", "green4", "steelblue"))


#Variability
#sample variance
var(plates$thickness)
tapply(plates$thickness, plates$day, var)

#standard deviation
sd(plates$thickness)

#range
diff(range(plates$thickness))
tapply(plates$thickness, plates$day, 
       function(x){
         diff(range(x))
       })

#MAD
mad(plates$thickness)

#quartiles
##Q1-Q3
quantile(plates$thickness, 0.25)
quantile(plates$thickness, 0.50)
quantile(plates$thickness, 0.75)

##min, Q1, median, mean, Q3, max
summary(plates$thickness)

##IQR = Q3-Q1
IQR(plates$thickness)
quantile(plates$thickness, 0.75) - quantile(plates$thickness, 0.25)

##分組計算IQR
tapply(plates$thickness, plates$day, IQR)


#Frequency Tables
plates$position <- cut(x = plates$thickness,
                       breaks = c(min(plates$thickness), 0.75,
                                  max(plates$thickness)),  #斷點
                       labels = c("bellow", "above"),  #標籤
                       include.lowest = TRUE)  #第一個區間是否包含斷點
table(plates$position, plates$day)

##### CH5.2 #####
#超幾何分佈
#24片鋼板中，有7片不良品，隨機抽5片(取後不放回)，均為良品(即x=0)的機率為何？
dhyper(x = 0, m = 7, n = 17, k = 5)
# ==> Ans:14.56%

#二項式分配
#不良品機率為50%，從12片鋼板中，隨機抽5片(取後放回)，均為良品(即x=0)的機率為何？
dbinom(x = 0, size = 5, prob = 0.5)
# ==> Ans:3.13%

#抽出不良品個數>=1個機率為何？
pbinom(q = 1, size = 5, prob = 0.5, lower.tail = FALSE)
# ==> Ans:81.3%


#卜瓦松分佈
#假設不良品發生機率為0.2，下次觀察中，不良品發生次數為0(即x=0)的機率為何？
dpois(x = 0, lambda = 0.2)
# ==> Ans:81.9%


#常態分佈
tmeans <- data.frame(
  Mean = tapply(plates$thickness, plates$day, mean),
  StdDev = tapply(plates$thickness, plates$day, sd))
tmeans
#        Mean     StdDev
#Day1 0.8315833 0.06767766
#Day2 0.7438333 0.04060863
pnorm(q = 0.75,
      mean = tmeans$Mean[1],
      sd = tmeans$StdDev[1])
# ==> 0.1140111 (Day1：11.40%)
pnorm(q = 0.75,
      mean = tmeans$Mean[2],
      sd = tmeans$StdDev[2])
# ==> 0.5603498 (Day2：56.03%)


#non-normal distributions
Days345 <- c(0.608, 0.700, 0.864, 0.643, 1.188, 0.610,
             0.741, 0.646, 0.782, 0.709, 0.668, 0.684,
             1.034, 1.242, 0.697, 0.689, 0.759, 0.700,
             0.604, 0.676, 0.687, 0.666, 0.612, 0.638,
             0.829, 0.838, 0.944, 0.829, 0.826, 0.649,
             0.702, 0.764, 0.873, 0.784, 0.697, 0.658)

library(qcc)
qcc(Days345, type = "xbar.one")

library(MASS)
boxcox(Days345 ~ 1, lambda = seq(-5, 5, 0.1))

library(car)
d345.trans <- powerTransform(Days345)

summary(d345.trans)
# ==> lambda = -3.1506, 95%CI -5.1346 ~ -1.1666 (include-2)
d345.lambda <- coef(d345.trans, round = TRUE)
d345.lambda
# ==> lambda = -2

transformed.Days345 <- bcPower(Days345,
                               lambda = d345.lambda)
qcc(transformed.Days345, type = "xbar.one")
# ==> all data fitting normal distribution
##### CH5.3 #####
# Proportion Confidence Interval
prop.test(6, 12)
# ==> 95%CI (0.2537816, 0.7462184)
#conf.level：信心水準(預設為0.95)

binom.test(6, 12)
# ==> 95%CI (0.2109446, 0.7890554)
# exact test

library(Hmisc)
binconf(6, 12, method = "all")
#             PointEst     Lower      Upper
#Exact           0.5    0.2109446   0.7890554 ==> F distribution(金標準)，缺點：推估較為保守
#Wilson          0.5    0.2537816   0.7462184 ==> (defalt)score-test-based，估計區間較窄、樣本數小(N<40)也適用
#Asymptotic      0.5    0.2171036   0.7828964 ==> 漸近分析，較為嚴謹，但容易有過度調整(校正)情形


#Mean Confidence Interval
ci.day1 <- t.test(day1)
ci.day1$conf.int

ci.day2 <- t.test(day2)
ci.day2$conf.int


#Variance Confidence Interval
day1.var.ll <- ((12-1)*var(day1))/(qchisq(0.975, (12-1)))
day1.var.ul <- ((12-1)*var(day1))/(qchisq(0.025, (12-1)))
day2.var.ll <- ((12-1)*var(day2))/(qchisq(0.975, (12-1)))
day2.var.ul <- ((12-1)*var(day2))/(qchisq(0.025, (12-1)))
cat("Day 1:\n", round(c(day1.var.ll,day1.var.ul), 3))
# ==> 95%CI (0.002, 0.013)
cat("Day 2:\n", round(c(day2.var.ll,day2.var.ul), 3))
# ==> 95%CI (0.001, 0.005)


#Hypothesis Testing ==> Mean
t.test(x = day1, y = day2, 
       alternative = "greater") # "greater":x has a larger mean than y <=> "less"：x has a smaller mean than y
# ==> p-value = 0.0005841 <0.05，拒絕虛無假設，day1鋼板平均厚度大於day2


#Hypothesis Testing ==> Variances
var.test(x = day1, y = day2)
# ==> p-value = 0.1046 >0.05，接受虛無假設，day1鋼板厚度變異情形和day2沒有顯著差異


#Hypothesis Testing ==> Proportions
prop.test(x = c(6, 1), n = c(12, 12))
# ==> p-value = 0.07244 >0.05，接受虛無假設，day1鋼板厚度異常比率和day2沒有顯著差異


#Hypothesis Testing ==> Normality
shapiro.test(day1)
# ==> p-value = 0.9177 >0.05，接受虛無假設，day1鋼板厚度呈常態分佈
qqnorm(day1, pch = 16, col = gray(0.4))
grid()
qqline(day1)

shapiro.test(day2)  #常態
shapiro.test(Days345)  #非常態
qqnorm(Days345, pch = 16, col = gray(0.4))
grid()
qqline(Days345)
