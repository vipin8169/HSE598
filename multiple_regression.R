setwd("C:/Users/vipin/Desktop/Rscripts")

# Clear your workspace if you still have stuff in your environment from the last session
rm(list = ls(all = TRUE))

# Install  package
install.packages("readxl")

# Load packages
library(readxl)
library(ggplot2)
library(gridExtra)
library(car)

require(data.table)
# Folds5x2_pp.xlsx is retrived from https://archive.ics.uci.edu/ml/machine-learning-databases/00294/CCPP.zip
powerPlantdata <- read_excel("Folds5x2_pp.xlsx")

summary(powerPlantdata)
fit1 <- lm(PE ~ AT + V + AP + RH, data=powerPlantdata)

summary(fit1)

# extract model coefficients from objects returned by modeling functions
coefficients(fit)

# confidence intervals for one or more parameters in a fitted model
confint(fit1, level=0.95)

# extract model residuals from objects returned by modeling functions
residuals(fit1)

# plot a histogram of residuals extracted above
hist(residuals(fit1), main="Histogram of residuals")

# construct component+residual plots (also called partial-residual plots
crPlots(fit1)

p1 <- ggplot(powerPlantdata, aes(y = PE, x = AT)) +
  geom_point(alpha=0.075)+
  geom_smooth(size=1)+
  theme_minimal()+
  labs (y = "Energy output (MW)", x = "Average temperature(°C)",
        title ="Power output vs. Temperature")
p2 <- ggplot(powerPlantdata, aes(y = PE, x = V)) +
  geom_point(alpha=0.075)+
  geom_smooth(size=1)+
  theme_minimal()+
  labs (y = "Energy output (MW)", x = "Exhaust Vacuum(cm Hg)",
        title ="Power output vs. Exhaust vacuum")
p3 <- ggplot(powerPlantdata, aes(y = PE, x = AP)) +
  geom_point(alpha=0.075)+
  geom_smooth(size=1)+
  theme_minimal()+
  labs (y = "Energy output (MW)", x = "Ambient Pressure(millibar)",
        title ="Power output vs. Ambient pressure")
p4 <- ggplot(powerPlantdata, aes(x = PE, y = RH)) +
  geom_point(alpha=0.075)+
  geom_smooth(size=1)+
  theme_minimal()+
  labs (y = "Energy output (MW)", x = "Relative Humidity(%)",
        title ="Power output vs. Relative humidity")

grid.arrange(p1, p2, p3, p4, ncol=2)


fit2 <- lm(PE ~ AT + V + AP, data=powerPlantdata)
fit3 <- lm(PE ~ AT + V + RH, data=powerPlantdata)
fit4 <- lm(PE ~ AT + RH + AP, data=powerPlantdata)
anova(fit1, fit2)
anova(fit1, fit3)
anova(fit1, fit4)
