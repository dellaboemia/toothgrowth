  # Coursera Data Science Specialization
# Statistic Inference Course Project
# ToothGrowth
# Author: John James
# Date: March 28, 2016
# toothgrowth.R

## ---- environment
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(pastecs)
library(RColorBrewer)
## ---- end

#######################################################################################
##                          EXPLORATORY DATA ANALYSIS                                ##
#######################################################################################
## ---- copyDataframe
tg <- data.frame(ToothGrowth)
tg$dose.supp  <- paste(tg$dose, tg$supp)
tg$dose       <- factor(tg$dose)
tg$dose.supp  <- factor(tg$dose.supp)
## ---- end

## ---- descStats

## Column indexes and names
colIndex <- c(1,3:5, 8:9,12:13)
colNames <- c("n", "NAs",  "Min","Max","Median","Mean","Var", "Std Dev")

## Overall Stats
tgStats <- tg$len
tgStats <- data.frame(stat.desc(tgStats))
colnames(tgStats) <- "Tooth Growth Data"
tgStats <- data.frame(t(tgStats))
tgStats <- tgStats[,colIndex]
colnames(tgStats) <- colNames


## Stats by Dose
dose0 <- subset(tg, dose == "0.5")$len
dose1 <- subset(tg, dose == "1")$len
dose2 <- subset(tg, dose == "2")$len
doses <- cbind(dose0, dose1, dose2)
doseStats <- stat.desc(doses)
colnames(doseStats) <- c("0.5mg", "1mg", "2mg")
doseStats <- t(doseStats)
doseStats <- doseStats[,colIndex]
colnames(doseStats) <- colNames

## Stats by Supp
supp0 <- subset(tg, supp == "OJ")$len
supp1 <- subset(tg, supp == "VC")$len
suppStats <- cbind(supp0, supp1)
suppStats <- stat.desc(suppStats)
colnames(suppStats) <- c("OJ", "Sol")
suppStats <- t(suppStats)
suppStats <- suppStats[,colIndex]
colnames(suppStats) <- colNames


## Stats by Treatment
treatment0 <- subset(tg, dose.supp == "0.5 OJ")$len
treatment1 <- subset(tg, dose.supp == "0.5 VC")$len
treatment2 <- subset(tg, dose.supp == "1 OJ")$len
treatment3 <- subset(tg, dose.supp == "1 VC")$len
treatment4 <- subset(tg, dose.supp == "2 OJ")$len
treatment5 <- subset(tg, dose.supp == "2 VC")$len
treatmentStats <- cbind(treatment0, treatment1, treatment2, treatment3, treatment4, treatment5)
treatmentStats <- stat.desc(treatmentStats)
colnames(treatmentStats) <- c("Treatment 0: 0.5mg with Orange Juice",
                              "Treatment 1: 0.5mg with Aqueous Solution",
                              "Treatment 2: 1mg with Orange Juice",
                              "Treatment 3: 1mg with Aqueous Solution",
                              "Treatment 4: 2mg with Orange Juice",
                              "Treatment 5: 2mg with Aqueous Solution")
treatmentStats <- t(treatmentStats)
treatmentStats <- treatmentStats[,colIndex]
colnames(treatmentStats) <- colNames
## ---- end


## ---- t.tests
## Difference in mean cell growth between doses
doseResults <- list()
doseResults[[1]] <- t.test(tg$len[tg$dose == "1"] - 
                           tg$len[tg$dose == "0.5"])
doseResults[[2]] <- t.test(tg$len[tg$dose == "2"] - 
                           tg$len[tg$dose == "1"])  

## Difference in mean cell growth between delivery methods
suppResults <- list()
suppResults[[1]] <- t.test(tg$len[tg$supp == "OJ"] - 
                           tg$len[tg$supp == "VC"])

## Difference in mean cell growth between treatments
treatmentResults <- list()
treatmentResults[[1]] <- t.test(tg$len[tg$dose.supp == "0.5 OJ"] - 
                                tg$len[tg$dose.supp == "0.5 VC"])
treatmentResults[[2]] <- t.test(tg$len[tg$dose.supp == "1 OJ"] - 
                                tg$len[tg$dose.supp == "1 VC"])
treatmentResults[[3]] <- t.test(tg$len[tg$dose.supp == "2 OJ"] - 
                                tg$len[tg$dose.supp == "2 VC"])
## ---- end

## ---- storeResults
alpha <- 0.05
testCols <- c("Test", "H0", "Ha", "Delta", "Lower CI", "Upper CI", "p.Value", "Result")
rFunc <- function(x,y) {
  if (x < y) {
    r <- "Reject"
  }else{
    r <- "Fail to Reject"
  }
  return(r)
}
## Dose Test Results
doseTest <- NULL
Test  <- c(1,2)
H0    <- c("u1 = u2", "u2 = u3")
Ha    <- c("u1 <> u2", "u2 <> u3")
Delta <- c(round(doseResults[[1]]$estimate,2),round(doseResults[[2]]$estimate,2))
CIl   <- c(round(doseResults[[1]]$conf.int[1],2),round(doseResults[[2]]$conf.int[1],2))
CIu   <- c(round(doseResults[[1]]$conf.int[2],2), round(doseResults[[2]]$conf.int[2],2))
pv    <- c(doseResults[[1]]$p.value, doseResults[[2]]$p.value)
Result <- c(rFunc(doseResults[[1]]$p.value, alpha), rFunc(doseResults[[2]]$p.value, alpha))
doseTest <- data.frame(Test, H0, Ha, Delta, CIl, CIu, pv, Result, row.names = NULL)
colnames(doseTest) <- testCols

## Delivery Method Test Results
suppTest <- NULL
rownames(suppTest) <- NULL
Test  <- 1
H0    <- c("u1 = u2")
Ha    <- c("u1 <> u2")
Delta <- round(suppResults[[1]]$estimate,2)
CIl   <- round(suppResults[[1]]$conf.int[1],2)
CIu   <- round(suppResults[[1]]$conf.int[2],2)
pv    <- suppResults[[1]]$p.value
Result <- rFunc(suppResults[[1]]$p.value, alpha)
suppTest <- data.frame(Test, H0, Ha, Delta, CIl, CIu, pv, Result, row.names = NULL)
colnames(suppTest) <- testCols

## Treatment Test Results
treatmentTest <- NULL
rownames(treatmentTest) <- NULL
Test  <- c(1,2,3)
H0    <- c("u1 = u2", "u3 = u4", "u5 = u6")
Ha    <- c("u1 <> u2", "u3 <> u4", "u5 <> u6")
Delta <- c(round(treatmentResults[[1]]$estimate,2),
           round(treatmentResults[[2]]$estimate,2),
           round(treatmentResults[[3]]$estimate,2))
CIl   <- c(round(treatmentResults[[1]]$conf.int[1],2),
           round(treatmentResults[[2]]$conf.int[1],2),
           round(treatmentResults[[3]]$conf.int[1],2))
CIu   <- c(round(treatmentResults[[1]]$conf.int[2],2), 
           round(treatmentResults[[2]]$conf.int[2],2),
           round(treatmentResults[[3]]$conf.int[2],2))
pv    <- c(treatmentResults[[1]]$p.value, 
           treatmentResults[[2]]$p.value,
           treatmentResults[[3]]$p.value)
Result <- c(rFunc(treatmentResults[[1]]$p.value, alpha), 
            rFunc(treatmentResults[[2]]$p.value, alpha),
            rFunc(treatmentResults[[3]]$p.value, alpha))
treatmentTest <- data.frame(Test, H0, Ha, Delta, CIl, CIu, pv, Result, row.names = NULL)
colnames(treatmentTest) <- testCols
## ---- end

## ---- power
alpha   <- .05
sigma   <- sd(tg$len)
delta   <- seq(0,10,1)
pwr     <- NULL
pwr     <- data.frame("Sample" = integer(0), "Delta" = numeric(0), "Power" = numeric(0))

### Power Curve n = 10
n     <- 10 
for (i in 1:11){
  del   <- delta[i]
  p     <- power.t.test(n = n, delta = del, sd = sigma, type = "two.sample", alt = "two.sided")$power
  del10 <- power.t.test(n = n, power = 0.8, sd = sigma, type = "two.sample", alt = "two.sided")$delta
  pwr   <- rbind(pwr,c(n,del,p))
}
### Power Curve n = 20
n     <- 20 
for (i in 1:11){
  del   <- delta[i]
  p     <- power.t.test(n = n, delta = del, sd = sigma, type = "two.sample", alt = "two.sided")$power  
  del20 <- power.t.test(n = n, power = 0.8, sd = sigma, type = "two.sample", alt = "two.sided")$delta
  pwr   <- rbind(pwr,c(n,del,p))
}

### Power Curve n = 30
n     <- 30 
for (i in 1:11){
  del   <- delta[i]
  p     <- power.t.test(n = n, delta = del, sd = sigma, type = "two.sample", alt = "two.sided")$power
  del30 <- power.t.test(n = n, power = 0.8, sd = sigma, type = "two.sample", alt = "two.sided")$delta
  pwr   <- rbind(pwr,c(n,del,p))
}
colnames(pwr) <- c("Sample", "Delta", "Power")
## ---- end

#######################################################################################
##                                      PLOTS                                        ##
#######################################################################################                  
## ---- boxPlots
bpByDose <- ggplot(data = tg, aes(x = dose, y = len)) + 
            geom_boxplot(aes(fill=factor(dose))) +
            ylab("Length") +
            xlab("Dose") +
            theme_bw() +
            ggtitle("Dose") +
            scale_fill_brewer(name = "Dose", palette = "Dark2") 
  
bpBySupp <- ggplot(data = tg, aes(x = supp, y = len)) + 
            geom_boxplot(aes(fill=factor(supp))) +
            ylab("Length") +
            xlab("Delivery") +
            theme_bw() +
            ggtitle("Delivery Method") +
            scale_fill_brewer(name = "Delivery\nMethod", palette = "Dark2") 

grid.arrange(bpByDose, bpBySupp, ncol = 2, top = "Odontoblast Cell Growth by Dose and Delivery Method")


bpByTreatment <- ggplot(data = tg, aes(x = dose.supp, y = len)) + 
            geom_boxplot(aes(fill=factor(dose.supp))) +
            ylab("Length") +
            xlab("Treatment") +
            theme_bw() +
            ggtitle("Odontoblast Cell Growth by Treatment") +
            scale_fill_brewer(name = "Treatment", palette = "Dark2") 

print(bpByTreatment)
## ---- end    

## ---- distributions
## By Dose
distAll <- ggplot(tg, aes(len, fill = ..x..)) +
  geom_histogram(binwidth = 1)  +
  scale_fill_gradient("Length", low = "orange", high = "red") +
  xlab("Length") +
  ylab("Count") +
  ggtitle("All doses") +
  theme_bw()

distD1 <- ggplot(subset(tg,dose == "0.5"), aes(len, fill = ..x..)) +
  geom_histogram(binwidth = 1)  +
  scale_fill_gradient("Length", low = "orange", high = "red") +
  xlab("Length") +
  ylab("Count") +
  ggtitle("0.5 Mg") +
  theme_bw()

distD2 <- ggplot(subset(tg,dose == "1"), aes(len, fill = ..x..)) +
  geom_histogram(binwidth = 1)  +
  scale_fill_gradient("Length", low = "orange", high = "red") +
  xlab("Length") +
  ylab("Count") +
  ggtitle("1 Mg") +
  theme_bw()

distD3 <- ggplot(subset(tg,dose == "2"), aes(len, fill = ..x..)) +
  geom_histogram(binwidth = 1)  +
  scale_fill_gradient("Length", low = "orange", high = "red") +
  xlab("Length") +
  ylab("Count") +
  ggtitle("2 Mg") +
  theme_bw()

## By Delivery Method
distS1 <- ggplot(subset(tg,supp == "OJ"), aes(len, fill = ..x..)) +
  geom_histogram(binwidth = 1)  +
  scale_fill_gradient("Length", low = "orange", high = "red") +
  xlab("Length") +
  ylab("Count") +
  ggtitle("Orange Juice") +
  theme_bw()

distS2 <- ggplot(subset(tg,supp == "VC"), aes(len, fill = ..x..)) +
  geom_histogram(binwidth = 1)  +
  scale_fill_gradient("Length", low = "orange", high = "red") +
  xlab("Length") +
  ylab("Count") +
  ggtitle("Aqueous Solution") +
  theme_bw()

grid.arrange(distAll, distD1, distD2, distD3, distS1, distS2, ncol = 2, top = "Distribution of Odontoblasts Cell Length by Dose & Delivery Method")
## ---- end

## ---- plotPowerCurve
pcurve <- ggplot(pwr, aes(x = Delta, y = Power, group = Sample, colour = Sample)) +
  geom_line() + 
  geom_hline(aes(yintercept = 0.8)) + 
  xlab("Delta") +
  ylab("Power") +
  theme_bw()
print(pcurve)

## ---- end
