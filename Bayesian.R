setwd("C:/Users/vipin/Desktop/Rscripts")

# Clear your workspace if you still have stuff in your environment from the last session
rm(list = ls(all = TRUE))

# Load packages
library(stats)
library(stringr)

# generate a sequence of numbers from 0 to 1, with an increment of 0.1
x=seq(0,1,by=0.1)
# divide the output space in a 1 by 2 matrix, to plot two graphs at once
par(mfrow=c(1,2))
alpha=c(13.8,93.8)
beta=c(9.2,29.2)
z=c(80,160)
N=c(100,200)
for(i in 1:length(alpha)){
  # generate the beta distibution for the given shape values (α and β)
  y<-dbeta(x,shape1=alpha[i],shape2=beta[i])
  # plot the probability density functions
  plot(x,y,type="l",xlab = "theta",ylab = "density", main = ifelse(i == 1,"prior","posterior"), sub=str_c("N=", N[i], ", z=",z[i] ))
}

# divide the output space in a 2 by 2 matrix, to plot four graphs at once
par(mfrow=c(2,2))
alpha=c(13.8,93.8, 173.8, 333.8)
beta=c(9.2,29.2, 49.2, 89.2)
z=c(80,160,320,640)
N=c(100,200,400,800)
for(i in 1:length(alpha)){
  y<-dbeta(x,shape1=alpha[i],shape2=beta[i])
  plot(x,y,type="l",xlab = "theta",ylab = "density", main = ifelse(i == 1,"prior","posterior"), sub=str_c("N=", N[i], ", z=",z[i] ))
}
