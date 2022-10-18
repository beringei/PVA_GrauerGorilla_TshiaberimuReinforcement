## Set the working directory:
workingDir <- "/Users/neethaiyer/Box Sync/DATA_PVA_2017-2022/JWM_PVA_CODE"
setwd(workingDir)
workingDir_Output <- "/Users/neethaiyer/Box Sync/DATA_PVA_2017-2022/JWM_PVA_CODE/PVA_Output/"
workingDir_Input <- "/Users/neethaiyer/Box Sync/DATA_PVA_2017-2022/JWM_PVA_CODE/PVA_Input/"
workingDir_Figures <- "/Users/neethaiyer/Box Sync/DATA_PVA_2017-2022/JWM_PVA_CODE/PVA_Figures/"
workingDir_Results <- "/Users/neethaiyer/Box Sync/DATA_PVA_2017-2022/JWM_PVA_CODE/PVA_Output/Results/"
##workingDir <- "~/Documents/git repositories/JWM_PVA_CODE/"

##### SELECT COLOR for graphs #### ----
## Color for LM projections:
colLM <- "#969696"
colLM_alpha <- rgb(150, 150, 150, alpha=150, maxColorValue = 255)
## Color for IBM projections:
colIBM <- "#de2d26"
colIBM_alpha <- rgb(222, 45, 38, alpha=150, maxColorValue = 255) 

## Look at your color palette you just chose:
## plot(rep(1,4), col=c(colLM_alpha, colLM, colIBM_alpha, colIBM), pch=19 ,cex=3)

## Read all csv files. 
## Your life history tables should have at least 3 columns: 
## age, mortality rate, and fertility rate
dat <- read.csv(paste0(workingDir_Input, "Gorilla_LifeTables.csv"))
dat$fertilityrate_MTN1 <- dat[,3]*.789 
## fertility rates multiplied by factor less than 1 to get eigen values of 1.01 which corresponds to a 1% growth rate
dat$fertilityrate_MTN2 <- dat[,3]*.643 
## fertility rates multiplied by factor less than 1 to get eigen values of 1.02 which corresponds to a 2% growth rate

## Tshiaberimu survey data: one column for year of census, one for the population count estimate
## References for these survey data can be found in Table S1 of the Supporting Information document
censusdat <- read.csv(paste0(workingDir_Input, "Tshiaberimu-SurveyData_1959-2021.csv"))

## probability of extinctions based on LM and IBM projections
mtn_3per_lm <- read.csv(paste0(workingDir_Output,"Results/Results_LM_mtn_3%.csv"))
mtn_2per_lm <- read.csv(paste0(workingDir_Output,"Results/Results_LM_mtn_2%.csv"))
mtn_1per_lm <- read.csv(paste0(workingDir_Output,"Results/Results_LM_mtn_1%.csv"))
mtn_3per_ibm <- read.csv(paste0(workingDir_Output,"Results/Results_IBM_mtn_3%.csv"))
mtn_2per_ibm <- read.csv(paste0(workingDir_Output,"Results/Results_IBM_mtn_2%.csv"))
mtn_1per_ibm <- read.csv(paste0(workingDir_Output,"Results/Results_IBM_mtn_1%.csv"))

## final population sizes based on LM and IBM projections
finalPop1 <- read.csv(paste0(workingDir_Output,"Results/Results_LM_Nfinal_mtn_3%.csv"))
finalPop2 <- read.csv(paste0(workingDir_Output,"Results/Results_LM_Nfinal_mtn_2%.csv"))
finalPop3 <- read.csv(paste0(workingDir_Output,"Results/Results_LM_Nfinal_mtn_1%.csv"))
finalPop4 <- read.csv(paste0(workingDir_Output,"Results/Results_IBM_Nfinal_mtn_3%.csv"))
finalPop5 <- read.csv(paste0(workingDir_Output,"Results/Results_IBM_Nfinal_mtn_2%.csv"))
finalPop6 <- read.csv(paste0(workingDir_Output,"Results/Results_IBM_Nfinal_mtn_1%.csv"))

########## Historical population trends for Tshiaberimu gorillas ########## ----
## Take a look at the historical population trajectories using Tshiaberimu census data

year <- censusdat$year ## census years
N <- censusdat$N ## census data
censusPeriod <- censusdat$year[1]:censusdat$year[nrow(censusdat)] ## vector for census period

## let's look at the rate of change in this population
## lambda is the finite rate of increase of a population over one time step. r is the intrinsinc rate of growth. negative r values indicate a population in decline. lambda < 1 indicates a decline. the relationship between lambda and r : lambda = Nt+1  / Nt, r = ln(lambda), lambda = e^r
numYears <- (length(censusPeriod)-1)
logLambda <- (1/numYears)*log(N[length(N)]/N[1]) ## loglambda = 1/timeperiod*log(Ntfinal)/Nt0
lambda <- exp(logLambda)
popEst <- N[1]*(exp(logLambda))^(0:numYears) ## this is the expected rate of change in the population given Ntfinal and Nt0
popEst ## these are the predicted population estimates given the calculated lambda value

## let's fit these parameter estimates to a linear model to calculate the r and lambda values to get a more accurate estimate of these parameters:
modelGeom <- lm(log(N)~year) ## should be linear on a log scale
r_lm <- modelGeom$coef[2] ## take the slope of the line from this linear model for the intrinsic rate of growth r=-0.0297
lambda_lm <- exp(modelGeom$coef[2]) ## lambda=0.97

######################### FIGURE 3: HISTORICAL POPULATION TRAJECTORY  ########################## ----
## Plot the historical population trends and the intrinsic growth rate of this population based on surveys
## Projected until 2021
## vector for census year axis to be plotted
censusAxis <- censusdat$year[1]:censusdat$year[nrow(censusdat)] 
projectionYears <- length(censusAxis)-1
popEst_lm <- N[1]*(exp(r_lm))^(0:projectionYears)

## Select file name:
file_name <- "Fig3_HistoricalTshiaberimuPopulation.jpg"

setwd(workingDir_Figures)
##pdf(file_name, width=5,height=5)
jpeg(file_name, width=5, height=5, units="in", res=1000)
par(oma=c(1,1,1,1), mar=c(5,4,2,1))
## plot the actual population sizes from census data and the expected population size:
plot(year, N, 
     xlab="Census year", ylab="Population size", 
     pch=21, type="o",bg="gray",col="black",
     ylim=c(0,40), 
     cex.lab=0.8, cex.axis=0.8, font.lab=2, axes=FALSE)
box(bty="l")
axis(2, font.lab=2, at=seq(0, 50, by=10), labels=seq(0, 50, by=10), las=2)
axis(1, font.lab=2, at=seq(censusAxis[1]+1, censusAxis[length(censusAxis)]+1, by=10), 
     labels=seq(censusAxis[1]+1, censusAxis[length(censusAxis)]+1, by=10), las=1)
lines(censusAxis, popEst_lm, col=colIBM, lty=2, lwd=2)
dev.off()

######################### FIGURE 4: EXTINCTION RISK PLOTS  ########################## ----

## Select file name:
file_name <- "Fig4_ExtinctionProbabilities_LM_IBM.jpg"

setwd(workingDir_Figures)
##pdf(file_name, width=8,height=4)
jpeg(file_name, width=8, height=3.5, units="in", res=1000)
layout(matrix(c(1,2,3,4,4,4), ncol=3, byrow=TRUE), heights=c(0.8,0.2))
par(oma=c(1, 1, 1, 1), mar=c(4, 4, 2, 2))

plot(mtn_1per_lm$extn_Risk, bg=colLM, type="o", pch=21, 
     xlab=NA, ylab=NA, xlim=c(1,9), ylim=c(0,50), 
     xaxt="n", yaxt = "n", axes=FALSE)
lines(mtn_1per_ibm$extn_Risk, bg=colIBM, type="o", pch=23)
box(bty="l")
axis(1, font.lab=2, at=seq(1, 9, by=1), labels=LETTERS[1:9], las=1)
axis(2, font.lab=2, at=seq(0, 50, by=10), labels=seq(0, 50, by=10), las=2)
title(main="1% Population growth rate", ylab="Extinction risk (%)", 
      cex=0.8, cex.main=0.9, font.lab=2)

plot(mtn_2per_lm$extn_Risk, bg=colLM, type="o", pch=21, 
     xlab=NA, ylab=NA, xlim=c(1,9), ylim=c(0,50), 
     xaxt="n", yaxt = "n", axes=FALSE)
lines(mtn_2per_ibm$extn_Risk, bg=colIBM, type="o", pch=23)
box(bty="l")
axis(1, font.lab=2, at=seq(1, 9, by=1), labels=LETTERS[1:9], las=1)
axis(2, font.lab=2, at=seq(0, 50, by=10), labels=seq(0, 50, by=10), las=2)
title(main="2% Population growth rate", cex.main=0.9, font.lab=2)

plot(mtn_3per_lm$extn_Risk, bg=colLM, type="o", pch=21, 
     xlab=NA, ylab=NA, xlim=c(1,9), ylim=c(0,50), 
     xaxt="n", yaxt = "n", axes=FALSE)
lines(mtn_3per_ibm$extn_Risk, bg=colIBM, type="o", pch=23)
box(bty="l")
axis(1, font.lab=2, at=seq(1, 9, by=1), labels=LETTERS[1:9], las=1)
axis(2, font.lab=2, at=seq(0, 50, by=10), labels=seq(0, 50, by=10), las=2)
title(main="3.2% Population growth rate", cex.main=0.9, font.lab=2)

mtext("Reinforcement scenario", side=1, line=-6, outer=TRUE, cex=0.7, font=2)

par(mai=c(0,0,0,0))
plot.new()
legend("bottom", legend=c("Leslie matrix", "Individual-based model"), 
       lty=c(1,1), pch=c(21,23), pt.bg=c(colLM,colIBM), 
       text.font=2, xpd = TRUE, horiz = FALSE, 
       inset = c(0, 0.04), bty = "y")
dev.off()

# xpd = TRUE: legend will go outside the plotting region 
# inset = c(x,y): how to move the legend relative to the 'bottom' of the plotting area

######################### FIGURE 5: FINAL POPULATION SIZES ########################## ----

## Select file name:
file_name <- "Fig5_FinalPopulationSize_LM_IBM.jpg"
setwd(workingDir_Figures)

##pdf(file_name, width=12, height=6)
jpeg(file_name, width=12, height=6, units="in", res=1000)
layout(matrix(c(1,2,3,4,5,6,7,7,7), ncol=3, byrow=TRUE), heights=c(0.45,0.45,0.1))
par(oma=c(1, 1, 1, 1), mar=c(4, 4, 2, 2))

t <- boxplot(finalPop3$A, finalPop3$B, finalPop3$C, 
             finalPop3$D, finalPop3$E, finalPop3$F, 
             finalPop3$G, finalPop3$H, finalPop3$I, 
             names=LETTERS[1:9], 
             pch=20, cex=1.2, col=colLM, outcol=colLM_alpha, 
             varwidth=FALSE, medlwd=1, medcol="white", 
             boxlty=1, whisklty=1, staplelty=0, ylim=c(0,200), 
             yaxt = "n", axes=FALSE)
box(bty="l")
axis(2, font.lab=2, at=seq(0, 200, by=50), labels=seq(0, 200, by=50), las=2)
axis(1, font.lab=2, at=seq(1, 9, by=1), labels=LETTERS[1:9], las=1)
title(main="1% Population growth rate", sub="Leslie matrix", font.lab=2)

t <- boxplot(finalPop2$A, finalPop2$B, finalPop2$C, 
             finalPop2$D, finalPop2$E, finalPop2$F, 
             finalPop2$G, finalPop2$H, finalPop2$I, 
             names=LETTERS[1:9], 
             pch=20, cex=1.2, col=colLM, outcol=colLM_alpha, 
             varwidth=FALSE, medlwd=1, medcol="white", 
             boxlty=1, whisklty=1, staplelty=0, ylim=c(0,200), 
             yaxt = "n", axes=FALSE)
box(bty="l")
axis(2, font.lab=2, at=seq(0, 200, by=50), labels=seq(0, 200, by=50), las=2)
axis(1, font.lab=2, at=seq(1, 9, by=1), labels=LETTERS[1:9], las=1)
title(main="2% Population growth rate", sub="Leslie matrix", font.lab=2)

t <- boxplot(finalPop1$A, finalPop1$B, finalPop1$C, 
             finalPop1$D, finalPop1$E, finalPop1$F, 
             finalPop1$G, finalPop1$H, finalPop1$I, 
             names=LETTERS[1:9], 
             pch=20, cex=1.2, col=colLM, outcol=colLM_alpha, 
             varwidth=FALSE, medlwd=1, medcol="white", 
             boxlty=1, whisklty=1, staplelty=0, ylim=c(0,200), 
             yaxt = "n", axes=FALSE)
box(bty="l")
axis(2, font.lab=2, at=seq(0, 200, by=50), labels=seq(0, 200, by=50), las=2)
axis(1, font.lab=2, at=seq(1, 9, by=1), labels=LETTERS[1:9], las=1)
title(main="3.2% Population growth rate", sub="Leslie matrix", font.lab=2)

t <- boxplot(finalPop6$A, finalPop6$B, finalPop6$C, 
             finalPop6$D, finalPop6$E, finalPop6$F, 
             finalPop6$G, finalPop6$H, finalPop6$I, 
             names=LETTERS[1:9], 
             pch=18, cex=1.2, col=colIBM, outcol=colIBM_alpha, 
             varwidth=FALSE, medlwd=1, medcol="white", 
             boxlty=1, whisklty=1, staplelty=0, ylim=c(0,200), 
             yaxt = "n", axes=FALSE)
box(bty="l")
axis(2, font.lab=2, at=seq(0, 200, by=50), labels=seq(0, 200, by=50), las=2)
axis(1, font.lab=2, at=seq(1, 9, by=1), labels=LETTERS[1:9], las=1)

t <- boxplot(finalPop5$A, finalPop5$B, finalPop5$C, 
             finalPop5$D, finalPop5$E, finalPop5$F, 
             finalPop5$G, finalPop5$H, finalPop5$I, 
             names=LETTERS[1:9], 
             pch=18, cex=1.2, col=colIBM, outcol=colIBM_alpha, 
             varwidth=FALSE, medlwd=1, medcol="white", 
             boxlty=1, whisklty=1, staplelty=0, 
             ylim=c(0,200), yaxt = "n", axes=FALSE)
box(bty="l")
axis(2, font.lab=2, at=seq(0, 200, by=50), labels=seq(0, 200, by=50), las=2)
axis(1, font.lab=2, at=seq(1, 9, by=1), labels=LETTERS[1:9], las=1)

t <- boxplot(finalPop4$A, finalPop4$B, finalPop4$C, 
             finalPop4$D, finalPop4$E, finalPop4$F, 
             finalPop4$G, finalPop4$H, finalPop4$I, 
             names=LETTERS[1:9], 
             pch=18, cex=1.2, col=colIBM, outcol=colIBM_alpha, 
             varwidth=FALSE, medlwd=1, medcol="white", 
             boxlty=1, whisklty=1, staplelty=0, 
             ylim=c(0,200), yaxt = "n", axes=FALSE)
box(bty="l")
axis(2, font.lab=2, at=seq(0, 200, by=50), labels=seq(0, 200, by=50), las=2)
axis(1, font.lab=2, at=seq(1, 9, by=1), labels=LETTERS[1:9], las=1)

mtext("Reinforcement scenario", side=1, line=-5, outer=TRUE, cex=1, font=2)
mtext("Population size after 50 years", side=2, line=-1, outer=TRUE, cex=1, font=2, las=0)

par(mai=c(0,0,0,0))
plot.new()
legend("bottom", legend=c("Leslie matrix","Individual-based model"), 
       fill=c(colLM,colIBM), text.font=2, 
       xpd = TRUE, horiz = FALSE, inset = c(0, 0.04), bty = "y")
dev.off()

###################### FIGURE S1: CUMULATIVE SURVIVAL PLOTS ######################### ----

## Eigenvector for mountain gorillas (same for 3%, 2%, and 1%)
n <- rep(1, nrow(dat))
n[1] <- 1
for (i in 2:length(n)){
  n[i] <- prod(1-dat[1:(i-1),2])
} ## Make sure sum equals 1 to generate pyramid
n_mtn <- n/(sum(n))
## for the cumulative survival curve:
n_mtnCS <- n/n[1]

## Select file name:
file_name <- "FigS1_CumulativeSurvival-Fertility.jpg"

setwd(workingDir_Figures)
##pdf(file_name, width=5,height=5)
jpeg(file_name, width=5, height=5, units="in", res=1000)
par(oma=c(1,1,1,1), mar=c(8,4,2,1))
plot(dat[,1], n_mtnCS, 
     type="o", pch=21, cex=0.6, bg="white", col=colLM,
     xlab="", ylab="",
     cex.axis=0.8, ylim=c(0,1), las=1, lwd=1.5)
title(xlab="Age (years)", line=2, font.lab=2, cex.lab=0.8)
title(ylab="Rate", line=2.5, font.lab=2, cex.lab=0.8)
points(dat[,1], dat[,3],
     type="o", pch=23, cex=0.6, bg="white", col=colIBM, lwd=1.5)
legend("bottom", legend=c("Female cumulative survival", "Female fertility"), 
       lty=c(1,1), lwd=c(1.5,1.5), pch=c(21,23), pt.bg=c("white","white"), col=c(colLM,colIBM),
       text.font=2, xpd = TRUE, horiz = FALSE, 
       inset = c(0, -0.5), bty = "y", cex=0.8)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
dev.off()

################## FIGURE S2-S4: LESLIE MATRIX PROJECTION PLOTS ##################### ----

## Select file name:
file_name <- "FigS2_LM_Projections_mtn1.jpg"
file_name <- "FigS3_LM_Projections_mtn2.jpg"
file_name <- "FigS4_LM_Projections_mtn3.jpg"

setwd(workingDir_Output)
## Select exticntion risk files:
probExt_lm <- read.csv("Results/Results_LM_mtn_1%.csv")
probExt_lm <- read.csv("Results/Results_LM_mtn_2%.csv")
probExt_lm <- read.csv("Results/Results_LM_mtn_3%.csv")

## Select the correct folder for different growth rates:
workingDir_LM <- ("LM_Projection_50year_mtn_1%")
workingDir_LM <- ("LM_Projection_50year_mtn_2%")
workingDir_LM <- ("LM_Projection_50year_mtn_3%")

setwd(workingDir_LM)
allScenarioFiles <- list.files(pattern="*.csv")

for (i in 1:length(allScenarioFiles)){
  assign(allScenarioFiles[i], 
         read.csv(allScenarioFiles[i], header=TRUE)
  )}

stochObjects <- c("LM_Stoch_Scenario1.csv","LM_Stoch_Scenario2.csv","LM_Stoch_Scenario3.csv",
                  "LM_Stoch_Scenario4.csv","LM_Stoch_Scenario5.csv","LM_Stoch_Scenario6.csv",
                  "LM_Stoch_Scenario7.csv","LM_Stoch_Scenario8.csv","LM_Stoch_Scenario9.csv")
detObjects <- c("LM_Det_Scenario1.csv","LM_Det_Scenario2.csv","LM_Det_Scenario3.csv",
                "LM_Det_Scenario4.csv", "LM_Det_Scenario5.csv","LM_Det_Scenario6.csv",
                "LM_Det_Scenario7.csv","LM_Det_Scenario8.csv","LM_Det_Scenario9.csv")

setwd(workingDir_Figures)
##pdf(file_name, width=10,height=10)
jpeg(file_name, width=10, height=10, units="in", res=1000)
par(mfrow=c(3,3), oma=c(0,0,0,0), mar=c(5,4,2,1), las=1, bty="l")
maxY <- 120 ## max y-axis value
time <- 0:50 ## time interval for the plots
for(j in 1:length(stochObjects)){
  probExt <- probExt_lm[j,3]
  scenario <- as.character(probExt_lm[j,1])
  tempX <- get(stochObjects[j])
  N_projected_detX <- unname(unlist(get(detObjects[j])))
  plot(N_projected_detX~time, type="l", col=1, 
       xlab="Years post-introduction", ylab="Population size", 
       ylim=c(0,maxY),lty=2, cex.lab=1, cex.axis=1, font.lab=2) ## plot of deterministic projection
  for(i in 1:ncol(tempX)){
    lines(time, tempX[,i], col=grey(.8, alpha=.05), lwd=3)
  } ## plots projections from stochastic LM simulations
  lines(apply(tempX, 1, mean)~time, type="l", col="white", lwd=3) ## plot mean projection from stochastic LM simulations
  ##lines(N_projected_detX~time, type="l", col=1, lwd=2, lty=4) ## replot deterministic projection
  title(main=paste0("Scenario ",scenario), sub=paste0("Extinction risk = ",probExt, "%"), 
        cex.main=1, cex.sub=1, col.sub=1, font.sub=3)
  qtiles <- apply(tempX, 1, function(v) quantile(v, probs=c(0.05, 0.95))) ## plot 95% confidence intervals for simulations
  lines((0:(nrow(tempX)-1)), qtiles[1,], col=colLM, lwd=3, lty=1)
  lines((0:(nrow(tempX)-1)), qtiles[2,], col=colLM, lwd=3, lty=1)
}
dev.off()

################### FIGURE 6, FIGURE S5-S6: IBM PROJECTION PLOTS #################### ----

setwd(workingDir_Output)
## Select file name:
file_name <- "Fig6_IBM_Projections_mtn1.jpg"
file_name <- "FigS5_IBM_Projections_mtn2.jpg"
file_name <- "FigS6_IBM_Projections_mtn3.jpg"

## Select exticntion risk files:
probExt_ibm <- read.csv("Results/Results_IBM_mtn_1%.csv")
probExt_ibm <- read.csv("Results/Results_IBM_mtn_2%.csv")
probExt_ibm <- read.csv("Results/Results_IBM_mtn_3%.csv")

## Select the correct folder for different growth rates:
workingDir_IBM <- ("IBM_Projection_50year_mtn_1%")
workingDir_IBM <- ("IBM_Projection_50year_mtn_2%")
workingDir_IBM <- ("IBM_Projection_50year_mtn_3%")

setwd(workingDir_IBM)
allScenarioFiles <- list.files(pattern="*.csv")

for (i in 1:length(allScenarioFiles)){
  assign(allScenarioFiles[i], 
         read.csv(allScenarioFiles[i], header=TRUE)
  )}

stochObjects <- c("IBM_Scenario1.csv","IBM_Scenario2.csv","IBM_Scenario3.csv",
                  "IBM_Scenario4.csv","IBM_Scenario5.csv","IBM_Scenario6.csv",
                  "IBM_Scenario7.csv","IBM_Scenario8.csv","IBM_Scenario9.csv")

timeunit <- 1/12 ## time interval for the plots

setwd(workingDir_Figures)
##pdf(file_name, width=10,height=10)
jpeg(file_name, width=10, height=10, units="in", res=1000)
par(mfrow=c(3,3), oma=c(0,0,0,0), mar=c(5,4,2,1), las=1, bty="l")
maxY <- 120 ## max y-axis value
for(j in 1:length(stochObjects)){
  probExt <- probExt_ibm[j,3]
  scenario <- as.character(probExt_ibm[j,1])
  resX <- get(stochObjects[j])
  
  plot((0:(nrow(resX)-1))*timeunit, apply(resX, 1, mean), type="l", col=2, lwd=2, 
       xlab="Years post-introduction", ylab="Population size", 
       font.lab=2, bty="l", ylim=c(0,120))
  for(i in 1:ncol(resX)){
    lines((0:(nrow(resX)-1))*timeunit, resX[,i], col=grey(.8, alpha=.05), lwd=3)
  }
  
  ## add mean trend
  lines((0:(nrow(resX)-1))*timeunit, apply(resX, 1, mean), type="l", col="white", lwd=3)
  
  ## add 95% upper/lower limits
  qtiles <- apply(resX, 1, function(v) quantile(v, probs=c(0.05, 0.95)))
  lines((0:(nrow(resX)-1))*timeunit, qtiles[1,], col=colLM, lwd=3, lty=1)
  lines((0:(nrow(resX)-1))*timeunit, qtiles[2,], col=colLM, lwd=3, lty=1)
  
  title(main=paste0("Scenario ",scenario), sub=paste0("Extinction risk = ",probExt, "%"), 
        cex.main=1, cex.sub=1, col.sub=1,  font.sub=3)
  
}
dev.off()

######################## REPORTING RESULTS FROM EACH MODEL ########################## ----

## For each scenario:
## Median and standard deviation Final Population Sizes for
## LM 3.2% growth rate
md3.2_lm <- round(apply(finalPop1,2,median), 1)
sd3.2_lm <- round(apply(finalPop1,2,sd), 1)
var3.2_lm <- round(apply(finalPop1,2,var), 1)

## LM 2% growth rate
md2_lm <- round(apply(finalPop2,2,median), 1)
sd2_lm <- round(apply(finalPop2,2,sd), 1)
var2_lm <- round(apply(finalPop2,2,var), 1)

## LM 1% growth rate
md1_lm <- round(apply(finalPop3,2,median), 1)
sd1_lm <- round(apply(finalPop3,2,sd), 1)
var1_lm <- round(apply(finalPop3,2,var), 1)

## IBM 3.2% growth rate
md3.2_ibm <- round(apply(finalPop4,2,median), 1)
sd3.2_ibm <- round(apply(finalPop4,2,sd), 1)
var3.2_ibm <- round(apply(finalPop4,2,var), 1)

## IBM 2% growth rate
md2_ibm <- round(apply(finalPop5,2,median), 1)
sd2_ibm <- round(apply(finalPop5,2,sd), 1)
var2_ibm <- round(apply(finalPop5,2,var), 1)

## IBM 1% growth rate
md1_ibm <- round(apply(finalPop6,2,median),1)
sd1_ibm <- round(apply(finalPop6,2,sd), 1)
var1_ibm <- round(apply(finalPop6,2,var), 1)

resultsReported <- cbind(md3.2_lm, sd3.2_lm, var3.2_lm, 
                         md2_lm, sd2_lm, var2_lm, 
                         md1_lm, sd1_lm, var1_lm,
                         md3.2_ibm, sd3.2_ibm, var3.2_ibm, 
                         md2_ibm, sd2_ibm, var2_ibm, 
                         md1_ibm, sd1_ibm, var1_ibm)
rm(list = c("md3.2_lm", "sd3.2_lm", "var3.2_lm", 
            "md2_lm", "sd2_lm", "var2_lm", 
            "md1_lm", "sd1_lm", "var1_lm",
            "md3.2_ibm", "sd3.2_ibm", "var3.2_ibm", 
            "md2_ibm", "sd2_ibm", "var2_ibm", 
            "md1_ibm", "sd1_ibm", "var1_ibm"))

setwd(workingDir_Results)
write.csv(resultsReported, file="Results_ReportedResults_median-sd-var.csv", row.names=F)
