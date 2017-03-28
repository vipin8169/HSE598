setwd("C:/Users/vipin/Desktop/Rscripts")

# Clear your workspace if you still have stuff in your environment from the last session
rm(list = ls(all = TRUE))

# Load packages
# library(readxl)
library(ggplot2)
library(gridExtra)
library(psych)

#tell where the data come from
dosageData=read.table("http://personality-project.org/r/datasets/R.appendix1.data",header=T)   #read the data into a table

anovaDosage = aov(Alertness~Dosage,data=dosageData)  #do the analysis of variance
#show the summary table
summary(anovaDosage)
#report the means and the number of subjects/cell
boxplot(Alertness~Dosage,data=dosageData, col=(c("gold","darkgreen", "red")), xlab="Level of Dosage", ylab="Alertness")
#graphical summary appears in graphics window


recallData=read.table("http://personality-project.org/r/datasets/R.appendix3.data",header=T)   #read the data into a table
#show the data
recallData
anovaRecall = aov(Recall~Valence+Error(Subject/Valence),recallData)
summary(anovaRecall)
#report the means and the number of subjects/cell
#graphical output
boxplot(Recall~Valence,data=recallData, col=(c("gold","darkgreen", "red")), xlab="Valence", ylab="Recall")



mixedData=read.table("http://personality-project.org/r/datasets/R.appendix5.data",header=T)   #read the data into a table
#data.ex5                                      #show the data
anovaMixed = aov(Recall~(Task*Dosage)+Error(Subject/Task)+Dosage,data=mixedData)

summary(anovaMixed)             
print(model.tables(anovaMixed,"means"),digits=3)       
#report the means and the number of subjects/cell
boxplot(Recall~Task*Valence*Gender*Dosage,data=mixedData) 
#graphical summary of means of the 36 cells
boxplot(Recall~Task*Valence*Dosage,data=mixedData)
#graphical summary of means of  18 cells
#INTERACTIONS
par(mfrow=c(1,2))
interaction.plot(mixedData$Dosage,mixedData$Task,mixedData$Recall, xlab="Dosage", ylab="Mean Recall", col=c("red", "green"), trace.label="Task type")
interaction.plot(mixedData$Task,mixedData$Dosage,mixedData$Recall, xlab="Task type", ylab="Mean Recall", col=c("red", "green", "blue"), trace.label="Dosage")
