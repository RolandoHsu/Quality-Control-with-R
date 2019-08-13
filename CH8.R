library(SixSigma)
library(tidyverse)
library(ggplot2)

ss.data.thickness2
str(ss.data.thickness2)

library(lattice)
dotplot(thickness ~ shift | day,
        data = ss.data.thickness2,
        layout = c(7, 1))

ggplot(ss.data.thickness2, aes(x = shift, y = thickness))+
  geom_point(colour = "#2626FF")+
  facet_wrap( ~ day, ncol = 7)+
  theme(
    panel.grid.major = element_line(color = "grey"), # 主要網格線為灰色
    panel.background = element_rect(fill = "white"), # 調整背景顏色
    strip.background = element_rect(fill = "#FFE8BF", color = "#0A0A0A"), # 調整上圖的顏色跟邊框
    panel.border = element_rect(color = "#0A0A0A", fill = NA, size = 0.05) # 繪圖區邊框調整
  )

# Example 8.2 Process Yield 
nc <- sum(ss.data.thickness2$thickness > 0.8 | 
          ss.data.thickness2$thickness < 0.7)

nc / length(ss.data.thickness2$thickness)

# Example 8.3 Proportion of Defects 
hist <- hist(ss.data.thickness2$thickness, 
     main = "Histogram of thickness",
     col = "#00E6E6",
     xlab = "Thickness")
hist # 說明：區隔點、包含個數、密度、中點、Ｘ軸名稱、距離是否相同

library(nortest)
ad.test(ss.data.thickness2$thickness)

# Estimate the parameters of population
thick.mu <- mean(ss.data.thickness2$thickness)
thick.segma <- sd(ss.data.thickness2$thickness)

# 右邊落入拒絕域的機率
def.USL <- pnorm(q = 0.8,
                 mean = thick.mu,
                 sd = thick.segma,
                 lower.tail = FALSE)
# 左邊落入拒絕域的機率
def.LSL <- pnorm(q = 0.7,
                 mean = thick.mu,
                 sd = thick.segma)
# 雙尾機率
def.USL + def.LSL

# Metal plates thickness. Process Performance 
P.p <- (0.782 - 0.718) / (6 * thick.segma)
P.p

P.pkU <- (0.782 - thick.mu) / (3 * thick.segma)
P.pkU

P.pkL <- (thick.mu - 0.718) / (3 * thick.segma)
P.pkL

P.pk <- min(P.pkU, P.pkL)
P.pk

# Capability Indices 
library(qcc)
groups <- qcc.groups(ss.data.thickness2$thickness,
                     ss.data.thickness2$ushift)    # 直接對ss.data.thickness2$thickness 以ushift 為key 進行分組

myqcc <- qcc(data = groups, type = "xbar", plot = F) # plot= F 代表不要讓他做圖出來

process.capability(object = myqcc,
                   spec.limits = c(0.718, 0.782))


Obs_USL <- sum(ss.data.thickness2$thickness > 0.782) / length(ss.data.thickness2$thickness)
Obs_LSL <- sum(ss.data.thickness2$thickness < 0.718) / length(ss.data.thickness2$thickness)



