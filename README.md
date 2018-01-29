# SurvivalAnalysis403
This is a project I did involving survival analysis to see if color affected the time to finish a matching game.

#Setting up the dataset

library(readr)
library(survival)
PerfectionFlash_data_8_ <- read_csv("~/Stas 403 HW/second project/PerfectionFlash_data (8).csv")
View(PerfectionFlash_data_8_)
#Creating the KM Curve

KM.obj <- survfit(Surv(PerfectionFlash_data_8_$timeUsed)~PerfectionFlash_data_8_$matchingScheme, data = PerfectionFlash_data_8_)
plot(KM.obj, lty=1:2, xlab = "Time taken to complete game (in seconds)", ylab = "Survival Probability", ylim = c(0,1), main = "KM Curves for Two Different Shapesplosion Games")
legend(1,.2, c("Different Colors", "Same Color"), lty=1:2)

#Creating subsets of the dataset
SameColor <- subset(PerfectionFlash_data_8_, PerfectionFlash_data_8_$matchingScheme == "shape")
DiffColor <- subset(PerfectionFlash_data_8_, PerfectionFlash_data_8_$matchingScheme == "diffColor")
View(SameColor)
View(DiffColor)

#Putting the subsets into different objects

KM.obj1 <- survfit(Surv(SameColor$timeUsed)~1, data = SameColor)
KM.obj2 <- survfit(Surv(DiffColor$timeUsed)~1, data = DiffColor)

#How to plot a hazard function

plot.haz <- function(KM.obj, plot="TRUE") {
  ti <- summary(KM.obj)$time
  di <- summary(KM.obj)$n.event
  ni <- summary(KM.obj)$n.risk
  #Est Hazard Function
  est.haz <- 1:(length(ti)) 
  for (i in 1:(length(ti)-1))
    est.haz[i] <- di[i]/(ni[i]*(ti[i+1]-ti[i])) 
  est.haz[length(ti)] <- est.haz[length(ti)-1]
  
  if (plot=="TRUE"){
    plot(ti,est.haz,type="s",xlab="Time",
         ylab="Hazard Rate", 
         main=expression(paste(hat(h),(t)[KM]))) 
  }
  return(list(est.haz=est.haz, time=ti))
}

#Plotting the separate hazard functions

plot.haz(KM.obj1, plot= "TRUE")

plot.haz(KM.obj2, plot= "TRUE")

#How to plot a cummulative hazard function

plot.chaz <- function(KM.obj, plot= "TRUE") {
  ti <- summary(KM.obj)$time
  di <- summary(KM.obj)$n.event
  ni <- summary(KM.obj)$n.risk
  #Est Cummulative Hazard Function
  est.cum.haz <- 1:(length(ti))
  for(i in 1:(length(ti)))
    est.cum.haz[i] <- sum(di[1:i]/ni[1:i])
  plot.chaz <- 1:length(KM.obj$time)
  for (i in 1:length(plot.chaz)) 
    plot.chaz[i] <- sum((KM.obj)$n.event[1:i]/(KM.obj)$n.risk[1:i])
  if (plot=="TRUE") { 
    plot((KM.obj)$time,plot.chaz,type="s",xlab="Time", 
         ylab="Cumulative Hazard",main=expression(paste(hat(H),(t)["NA"]))) 
  }
  return(list(est.chaz=plot.chaz, time=(KM.obj)$time))
  
}
  
#plot of cummulative hazard functions

plot.chaz(KM.obj1, plot = "TRUE")

plot.chaz(KM.obj2, plot = "TRUE")

# Formal Log-rank test
survdiff(Surv(PerfectionFlash_data_8_$timeUsed)~PerfectionFlash_data_8_$matchingScheme, data = PerfectionFlash_data_8_, rho = 0)

#Wilcoxon test
survdiff(Surv(PerfectionFlash_data_8_$timeUsed)~PerfectionFlash_data_8_$matchingScheme, data = PerfectionFlash_data_8_, rho = 1)

