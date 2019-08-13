##### Cause-and-effect diagram 魚骨圖 ######

cManpower <- c("Recepcionist", "Record. Operator", "Storage operators")
cMaterials <- c("Supplier", "Transport agency", "Packing")
cMachines <- c("Compressor type", "Operation conditions", "Machine adjustment")
cMethods <- c("Reception", "Transport method")
cMeasurements <- c("Recording method", "Measurement appraisal")
cGroups <- c("Manpower", "Materials", "Machines", "Methods", "Measurements")
cEffect <- "Too high density"


library(qcc)
cause.and.effect(
  cause = list(Manpower = cManpower,
               Materials = cMaterials,
               Machines = cMachines,
               Methods = cMethods,
               Measurements = cMeasurements),
  effect = cEffect)

cause.and.effect(
  cause = list(Manpower = cManpower,
               Materials = cMaterials,
               Machines = cMachines,
               Methods = cMethods,
               Measurements = cMeasurements),
  effect = cEffect,
  title = "Cause-and-effect diagram(調參數)",
  cex = c(2,1.5,2),
  font = c(2,1,2))


library(SixSigma)
ss.ceDiag(
  effect = cEffect,
  causes.gr <- cGroups,
  causes = list(cManpower, cMaterials, cMachines,
                cMethods, cMeasurements),
  main = "Cause-and-effect diagram",
  sub = "Pellets Density")


##### Cheek Sheet ######
cManpower <- c("Recepcionist", "Record. Operator", "Storage operators")
cMaterials <- c("Supplier", "Transport agency", "Packing")
cMachines <- c("Compressor type", "Operation conditions", "Machine adjustment")
cMethods <- c("Reception", "Transport method")
cMeasurements <- c("Recording method", "Measurement appraisal")
cGroups <- c("Manpower", "Materials", "Machines", "Methods", "Measurements")
cEffect <- "Too high density"

# rbind: binds rows of data frames with identical columns to create larger data frames
data_checkSheet <- rbind(
  data.frame(Group = "Manpower",
             Cause = cManpower),
  data.frame(Group = "Machines",
             Cause = cMachines),
  data.frame(Group = "Materials",
             Cause = cMaterials),
  data.frame(Group = "Methods",
             Cause = cMethods),
  data.frame(Group = "Measurements",
             Cause = cMeasurements)
)

data_checkSheet$A_supplier <- NA
data_checkSheet$B_supplier <- NA
data_checkSheet$C_supplier <- NA

##### Hist #####
hist(pdensity)

par(bg = "gray95")
hist(pdensity,
     main = "Histogram of pellets density - Sample #25",
     sub = "Data from ceramic process",
     xlab = expression("Density (g"/"cm"^3*")"),
     col = "steelblue",
     border = "white",
     lwd = 2,
     las = 1,
     bg = "gray")


library(lattice)
histogram(pdensity,
          xlab = expression("Pellets density (g"/"cm"^3*")"),
          ylab = "Probability density",
          type = "density",
          panel = function(x, ...) {
            panel.histogram(x, ...)
            panel.mathdensity(dmath = dnorm,
                              col = "black",
                              lwd = 3,
                              args = list(mean = mean(x),
                                          sd = sd(x)))
          } )


library(ggplot2)
ggplot(data = data.frame(pdensity),
       aes(x = pdensity)) +
  geom_histogram(fill = "seagreen",  #column顏色
                 colour = "lightgoldenrodyellow",  #column框線顏色
                 binwidth = 0.2) +  #寬度
  labs(title = "Histogram",
       x = expression("Density ("*g/cm^3*")"),
       y = "Frequency")

##### Pareto Chart #####
data_checkSheet$A_supplier <- c(2, 0, 0, 2, 1, 7, 1,
                                3, 6, 0, 1, 2, 0)
data_checkSheet$B_supplier <- c(0, 0, 1, 1, 2, 1, 12,
                                1, 2, 1, 0, 0, 1)
data_checkSheet$C_supplier <- c(0, 1, 0, 6, 0, 2, 2,
                                4, 3, 0, 1, 0, 2)
data_checkSheet$Total <- data_checkSheet$A_supplier +
  data_checkSheet$B_supplier +
  data_checkSheet$C_supplier


data_pareto <- data_checkSheet[order(
  data_checkSheet$Total,
  decreasing = TRUE), ]
par(mar = c(8, 4, 4, 2) + 0.1)
#c(bottom, left, top, right)
#default is c(5, 4, 4, 2) + 0.1.
barplot(height = data_pareto$Total,  #y軸
        names.arg = data_pareto$Cause,  #x軸
        las = 2,  #寬度
        main = "Pareto chart for total causes")  #title


library(qcc)
data_pareto2 <- data_pareto$Total
names(data_pareto2) <- data_pareto$Cause
pareto.chart(data_pareto2, 
             main = "Out-of-control causes")


library(qualityTools)
paretoChart(x = data_pareto2,
            main = "Out-of-control causes")


library(qicharts)
spreadvector <- rep(names(data_pareto2),
                    times = data_pareto2)
paretochart(spreadvector)

##### Scatter Plot #####
set.seed(1234)
# rnorm(): normal standard distribution
ptemp <- - 140 + 15*pdensity + rnorm(24)

plot(pdensity ~ ptemp,
     col = "gray40",
     pch = 20,  #圖形
     lwd = 5,  #圖形大小
     main = "Pellets density vs. temperature",
     xlab = "Temperature (Celsius)",
     ylab = expression("Density ("*g/cm^3*")"))


library(lattice)
xyplot(pdensity ~ ptemp,
       xlab = "Temperature (Celsius)",
       ylab = expression("Density ("*g/cm^3*")"))


library(ggplot2)
ggplot(data = data.frame(pdensity, ptemp), 
       aes(x = ptemp, y = pdensity)) +
  geom_point() +
  labs(title = "Pellets density vs. temperature",
       x = "Temperature (Celsius)",
       y = expression("Density ("*g/cm^3*")"))

##### Stratification #####
psupplier <- rep(c("A", "B", "C"), each = 8)

boxplot(pdensity ~ psupplier,
        col = "gray70",
        xlab = "Supplier",
        ylab = expression("Density ("*g/cm^3*")"),
        main = "Box plots by supplier")


library(lattice)
bwplot(pdensity ~ psupplier,
       xlab = "Supplier",
       ylab = expression("Density ("*g/cm^3*")"))


library(ggplot2)
ggplot(data = data.frame(pdensity, psupplier), 
       aes(x = psupplier, y = pdensity)) + 
  geom_boxplot() +
  labs(title = "Box plots by supplier",
       x = "Supplier",
       y = expression("Density ("*g/cm^3*")"))
