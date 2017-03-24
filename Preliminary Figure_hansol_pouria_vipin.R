##############################################################################################
## o. Introduction   #########################################################################
#set Work Directory
setwd("P:/myr")
#install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("data.table")
install.packages("reshape2")
install.packages("GGally")
#clear work space
rm(list = ls(all = TRUE))
#load pachages 
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(reshape2)
library(GGally)

##############################################################################################
## 1. Data Cleaning   ########################################################################
#input data 
pima_diabetes.df <- fread("pima-indians-diabetes.data", header = F, sep = ',')
#inspect data
View(pima_diabetes.df)
head(pima_diabetes.df)
str(pima_diabetes.df)
#replace zero, which are actually missing values, with NA: for  blood pressure and BMI
pima_diabetes.df[pima_diabetes.df$V3==0] <- NA
pima_diabetes.df[pima_diabetes.df$V6==0] <- NA
#count missing values for all columns 
sapply(pima_diabetes.df, function(x) sum(is.na(x)))
#delete missing values 
pima_diabetes_clean.df <- na.omit(pima_diabetes.df)
#save cleaned data
save(pima_diabetes_clean.df, file="pima_diabetes_clean.df")
load('pima_diabetes_clean.df')
#assign  meaningful header to the data 
header <- c("Pregnancy",
            "Glucose",
            "Blood_pressure",
            "Skin_thickness",
            "Insulin",
            "BMI",
            "Pedigree",
            "Age",
            "Diabetes")
names(pima_diabetes_clean.df) <- header
View(pima_diabetes_clean.df)

##############################################################################################
## 2. EDA analysis   #########################################################################
attach(pima_diabetes_clean.df)
tempD<-pima_diabetes_clean.df
NewTempD <- tempD[,c("Pregnancy", "Blood_pressure", "Skin_thickness", "BMI", "Pedigree", "Age","Diabetes")]#selecting the non-medical variables to add in the EDA
NewTempD$Diabetes <- as.factor(NewTempD$Diabetes) #change the data-type of values under the "Diabetes" column as categorical so that they can be used to color the dots.
ggpairs(data = NewTempD,  mapping =  ggplot2::aes(colour = Diabetes)) #create scatterplots
ggsave("plot00.jpeg", width = 16, height = 12)

#For better inspection we added a contour(density) map
ggpairs(data = NewTempD,  mapping =  ggplot2::aes(colour = Diabetes), upper = list(continuous = "density")) #create scatterplots
ggsave("plot01.jpeg", width = 16, height = 12)

##############################################################################################
## 3.Local regression analysis   #########################################################################
plot1 <- ggplot(pima_diabetes_clean.df, aes(x = Pedigree, y = BMI, group=Diabetes, col=Diabetes, fill=Diabetes)) +
         geom_point(alpha=0.5)+
         geom_smooth(size=1)+
         theme_minimal()+
         labs (x = "Diabetes pedigree function", y = "Body mass index (kg/m^2)",
              title ="Predicting Diabetes Mellitus with Non-Medical Data",
              subtitle = "Hansol Rheem, Pouria Salehi & Vipin Verma",
              caption = "Source: https://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes")
plot1+scale_fill_gradient(low="#F8766D",high="#00BFC4")+scale_colour_gradient(low="#F8766D",high="#00BFC4")
ggsave("plot1.jpeg", width = 7, height = 6)

plot2 <- ggplot(pima_diabetes_clean.df, aes(x = Blood_pressure, y = BMI, group=Diabetes, col=Diabetes, fill=Diabetes)) +
         geom_point(alpha=0.5)+
         geom_smooth(size=1)+
         theme_minimal()+
         labs (x = "Diastolic blood pressure (mm Hg)", y = "Body mass index (kg/m^2)",
              title ="Predicting Diabetes Mellitus with Non-Medical Data",
              subtitle = "Hansol Rheem, Pouria Salehi & Vipin Verma",
              caption = "Source: https://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes")
plot2+scale_fill_gradient(low="#F8766D",high="#00BFC4")+scale_colour_gradient(low="#F8766D",high="#00BFC4")
ggsave("plot2.jpeg", width = 7, height = 6)

plot3 <- ggplot(pima_diabetes_clean.df, aes(x = Skin_thickness, y = Age, group=Diabetes, col=Diabetes, fill=Diabetes)) +
         geom_point(alpha=0.5)+
         geom_smooth(size=1)+
         theme_minimal()+
         labs (x = "Triceps skin fold thickness (mm)", y = "Age (years) ",
              title ="Predicting Diabetes Mellitus with Non-Medical Data",
              subtitle = "Hansol Rheem, Pouria Salehi & Vipin Verma",
              caption = "Source: https://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes")
plot3+scale_fill_gradient(low="#F8766D",high="#00BFC4")+scale_colour_gradient(low="#F8766D",high="#00BFC4")
ggsave("plot3.jpeg", width = 7, height = 6)




#http://ggobi.github.io/ggally/#canonical_correlation_analysis
#http://www.statmethods.net/graphs/scatterplot.html
#https://en.wikipedia.org/wiki/Local_regression
#http://stackoverflow.com/questions/29554796/meaning-of-band-width-in-ggplot-geom-smooth-lm
#http://stats.stackexchange.com/questions/82603/understanding-the-confidence-band-from-a-polynomial-regression
