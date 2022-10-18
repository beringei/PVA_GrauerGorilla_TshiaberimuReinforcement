## Set the working directory:
workingDir <- "/Users/neethaiyer/Box Sync/DATA_PVA_2017-2022/JWM_PVA_CODE"
##workingDir <- "~/Documents/git repositories/JWM_PVA_CODE/"


##################### PART 1: Leslie Matrix model (Simple PVA) ###################### ----

################### FUNCTION 1: CREATE LESLIE MATRICES FROM LIFE TABLES ################### ----

## Function to create the Leslie Matrix used in the LM projection
leslieMatrix <- function(lifetable, filename){
  mat <- matrix(0, nrow=nrow(lifetable), ncol=nrow(lifetable)) ## create an empty square matrix
  mat[1,] <- lifetable[,3] ## first row in matrix assigned the fertility rates from the life table
  mat2 <- matrix(0,ncol=ncol(mat)-1, nrow=ncol(mat)-1) 
  diag(mat2) <- 1-lifetable[-nrow(lifetable),2] ## survival rates are assigned to just under the diagonal of a LM
  mat[2:nrow(mat), 1:(ncol(mat)-1)] <- mat2
  write.csv(mat, file=filename, row.names=F)
}

#################### FUNCTION 2: DETERMINISTIC LESLIE MATRIX PROJECTION ################### ----

## The function below returns the deterministic LM population size projection
## using the projection period (tfinal), 
## age and number of females in the starting population (No), 
## and Leslie matrix (LM)
pop_projection <- function(tfinal, LM=mat, No=No){
  pop <- N <- No
  for (i in 1:tfinal){
    pop <- LM%*%pop
    N <- cbind(N,pop)
  }
  N
}

###################### FUNCTION 3: STOCHASTIC LESLIE MATRIX PROJECTION #################### ----

## The function below returns the stochastic LM population size projection
## using the projection period (tfinal),
## age and number of females in the starting population (No),
## and Leslie matrix (LM)
stoch_projection <- function(tfinal, LM=mat, No=No){
  pop <- N <- No
  for (i in 1:tfinal){
    currentpop <- pop
    for (j in 1:length(No)){
      pop[j] <- sum(rbinom(n=length(No), size=currentpop, prob=LM[j,]))
    }
    N <- cbind(N, pop)
  }
  N
}
  
################ PART 2: Individual Based Model (IBM) (Complex PVA) ################# ----

#################### FUNCTION 4: CREATE LIST OF REINTRODUCION SCENARIOS ################### ----

## Create a list of these scenarios with the age of individuals introduced, and their status at the start of the model
convertToList <- function(scenario, adultAge, weaningAge){
  res <- list()
  for(i in 1:ncol(scenario)){
    ages <- c(na.omit(scenario[,i]))
    ##ages <- ages[ages>=weaningAge]
    statuses <- ifelse(ages<adultAge, "I", "C")
    times <- numeric(length(ages))
    times <- ages-weaningAge
    times[ages>=adultAge] <- ages[ages>=adultAge]-adultAge
    res[[i]] <- data.frame(ages0 = ages, status0 = statuses, time0 = times)
  }
  return(res)
}

################### FUNCTION 5-10: TRANSITION PROBABILITES OF INDIVIDUALS ################# ----

## For this model, we specified 5 categories of individuals: 
## immature (I): either weaned juveniles or unweaned infants, cycling adult female (C), pregnant adult female (P), 
## lactating adult female (L), and females that lose their infants (CD)
## Thus, our IBM will include both an age and reproductive status category vector. 

## We specified the transition probabilities, in relation to time since entry in the "current" category, as functions:

#################### FUNCTION 5: Transition from I to C #################### ----

## Probability for an subadult to transition to adult is zero if time as subadult is t<5.5 and 1 if t>=5.5
IC <- function(t) ifelse(t<weaningAge+1, 0, 1) 

#################### FUNCTION 6: Transition from C to P #################### ----

## Probability for a cycling adult female to become pregnant, per time step, given the alpha value
CP <- function(t, alpha) 1-exp(log(1-alpha)/(1/timeunit)) 

## relationship between alpha and p: 
## (1-p)^(1/timeunit)=1-alpha
## log(1-p)=log(1-alpha)/(1/timeunit)
## p=1-exp(log(1-alpha)/(1/timeunit))
## alpha is probability to be pregnant after 1 year of cycling. 
## alpha tunes the inter=birth interval. 
## p is the alpha per month (probability to get pregnant per month)

#################### FUNCTION 7: Transition from P to L #################### ----

## Probability for a pregnant adult female to transition to "lactating" is 1 if she has been pregnant for 8.5 months
PL <- function(t) ifelse(t<8.5/12, 0, 1) 

#################### FUNCTION 8: Transition from L to C  #################### ----

## Case 1: Probability that a lactating female transitions to cycling is 1 after the weaning age of her infant
LC <- function(t) ifelse(t<weaningAge,0,1) 

## Case 2: Probability that a lactating female transitions to cycling due to the loss of her dependent infant
LCdeathInf <- function(t) 1-exp(log(1-datX[trunc(t+1),2])/(1/timeunit))  

######################## FUNCTION 9: Mortality rate  ######################## ----

## Mortality rate, per month, specified by the LM
deathRate <- function(age) 1-exp(log(1-datX[trunc(age+1),2])/(1/timeunit)) 

######################## FUNCTION 10: Status change  ######################## ----

## Function that specifies the status change of each agent in model : I,C,P,L,CD
statusChange <- function(status, t, alpha){
  if(status =="I"){
    if(rbinom(1,1,IC(t))){
      return("C")
    } else return('I')
  } else if (status =="C"){
    if(rbinom(1,1,CP(t, alpha))){
      return("P")
    } else return('C')
  } else if (status =="P") {
    if(rbinom(1,1,PL(t))){
      return("L")
    } else return('P')
  } else if(!rbinom(1,1,LC(t))){		    ## If her infant doesn't get weaned because it is too young
    if(!rbinom(1,1,LCdeathInf(t))) {	  ## If her infant doesn't die
      return("L")						            ## Then the female is still lactating
    } else return("CD")					        ## If her infant dies, female becomes "CD", i.e. she transitions to cycling after the death of her infant
  } else  return("C")                   ## We distinguish between CD & C because if the baby dies, don't add it into the population the next year
}

## Possible statuses: I,C,P,L,CD
## quick test:
## statusChange("L", 0.6, 0.99)
## statusChange("C", 3/12, 0.99)

################################ FUNCTION 11: STOCHASTIC IBM ############################### ----

simTshia <- function(ages0, status0, time0, nyears, timeunit, alpha, verbose=T){
  abmDataLog <- data.frame(timestep=0, ages=ages0, time=time0, status=status0, indiv=1:length(ages0), stringsAsFactors = FALSE)
  iter <- max(abmDataLog$indiv)+1 ## iter keeps track of the index of the last row (of abmDataLog) + 1, which is essentially the next available gorilla "ID" in the model. We don't want to repeat ID's. 
  for(i in 1:trunc(nyears/timeunit)){
    if(i%%(1/timeunit)==0 & verbose) print(paste("time =",i*timeunit)) ## %% or modulo find the remainder: every year, when you divide 1 by 12 %% = 0, so this keeps track of time in the simulation. if we select verbose=F, the time is not printed. 
    newAbmData <- abmDataLog[0,]
    abmData <- abmDataLog[abmDataLog$timestep==(i-1),] ## abmData keeps tracks of data from previous timestep
    for(j in 1:nrow(abmData)){ ## j is the focal individual
      currentStatus <- abmData[j,4] ## this is the status of the individual j
      newStatus <- statusChange(currentStatus, abmData[j,3], alpha=alpha) ## Returns "I", "P", "L", "C", or "CD" and newStatus is the status at the next time step
      if(rbinom(1,1,deathRate(abmData[j,2]))==1){ ## Did the individual just die?
        newStatus <- "D"
      }
      if(currentStatus!=newStatus) {
        newtime <- timeunit ## this will depend on the newStatus
        if(newStatus=="L") infantSex <- sample(0:1,1) ## this keeps track of sex, using sex ratio 1:1, where 0=Male, 1=Female
      } else {
        newtime <- abmData[j,3]+timeunit
      }
      newAbmData <- rbind(newAbmData, data.frame(timestep=i, ages=abmData[j,2]+timeunit, time=newtime, status=newStatus, indiv=abmData[j,5], stringsAsFactors = FALSE))
      if(currentStatus=="L" & newStatus=="C"){ ## 50/50 sex ratio
        if( infantSex==1) {
          newAbmData <- rbind(newAbmData, data.frame(timestep=i, ages=weaningAge+timeunit, time=weaningAge+timeunit, status="I", indiv=iter, stringsAsFactors = FALSE))
          iter <- iter+1
        }
      } ## If weaning, add new row to abmData
    }
    if(sum(newAbmData$status=="CD")>0) newAbmData$status[newAbmData$status=="CD"] <- "C"
    if(nrow(newAbmData[newAbmData$status!="D",])==0) break ## if the adult female died, newAbmData should have 1 row with status D, and the dependent infant dies too, so we break the loop and add nothing to abmDataLog. 
    abmDataLog <- rbind(abmDataLog, newAbmData[newAbmData$status!="D",]) ## if there are individuals that are not dead then add them to the newAbmData
  }
  return(abmDataLog)
}
