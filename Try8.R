# Fig phenology simulation

fig_sim <- function(psize, itermax, burnin, b_filename) {
  #load truncnorm package
  library(truncnorm)
  set.seed(254)
  iteration<-1
    
    while(iteration<=itermax){
      # Setting up by drawing reporductive plants
      # Start empty
      sumpopB<-c()
      sumpopC<-c()
      sumpopD<-c()
      sumR<-c()
      stateB<-c()
      stateC<-c()
      stateD<-c()

      # New variables for male success failure
      successM<-c()
      failureM<-c()
      sumpopsuccessM<-c()
      sumpopfailureM<-c()
      succpolr<-c()
      failpolr<-c()
      
      # New variables for female success failure
      successF<-c()
      failureF<-c()
      sumpopsuccessF<-c()
      sumpopfailureF<-c()
      
      # Alternative variables male reprod
      stateDs<-c()
      failureDs<-c()
      successDs<-c()
      sumpopsuccessDs<-numeric()
      sumpopfailureDs<-numeric()
      countsumpopsuccessDs<-numeric()
      countsumpopfailureDs<-numeric()
      
      # Update state_
      # If not reproductive then draw reproductive
      countsumpopD<-numeric()
      countsumpopC<-numeric()
      countsumpopB<-numeric()
      countsumpopR<-numeric()
      
      # New variables for female reprod
      countsumpopsuccessF<-numeric()
      countsumpopfailureF<-numeric()
      
      # New variables for male reprod
      countsumpopsuccessM<-numeric()
      countsumpopfailureM<-numeric()
      
      # Loop to ensure start only if wasps exist
      startR<-0
      
      while(startR==0){
      # To call for assign repord
      initialR <- rbinom(psize,1,4.03/365.25)
      initialR
      # Drawing repord phases of different durations
      # Adjust according to phenology
      # Results into vectors
      #sampleB <- rtruncnorm(psize,a=0,b=Inf,mean=1.43,sd=0.772)
      sampleB <- rtruncnorm(psize,a=0,b=Inf,mean=10.01,sd=5.404)
      #sampleC <- rtruncnorm(psize,a=0,b=Inf,mean=2.88,sd=0.795)
      sampleC <- rtruncnorm(psize,a=0,b=Inf,mean=20.16,sd=5.565)
      #sampleD <- rtruncnorm(psize,a=0,b=Inf,mean=1.22,sd=0.832)
      #add one day as pollinator viability considered
      #need to add as effectively affect pollination success
      sampleD <- rtruncnorm(psize,a=0,b=Inf,mean=9.54,sd=5.824)
      samplesum <- sampleB+sampleC+sampleD
      # Locating a random point in reproductive cycle
      pointintime<-runif(psize,min=0,max=samplesum)
      
      # Initiate phases 
      # Finding individuals in phase D
      # To attempt making reproductive states numeric
      remnantD<-pointintime-sampleB-sampleC-sampleD
      stateD <-ifelse(remnantD<=0 | initialR==0,  0, remnantD)
      # Finding individuals in phase C
      remnantC<-pointintime-sampleB-sampleC
      notC<-(remnantC<=0| initialR==0| remnantD>0)
      stateC<-ifelse(notC, 0, remnantC)
      # Finding individuals in phase B
      remnantB<-pointintime-sampleB
      notB<-(remnantB<=0| initialR==0| remnantC>0)
      stateB <- ifelse(notB, 0, remnantB)
      
      # Summarizing ind in Reprod
      # Number of reproductives assuming that each ind can occupy one state
      sumR<-(sum(stateB!=0)+sum(stateC!=0)+sum(stateD!=0))
      # Ensures that wasps exist to start
      startR<-(sum(stateC!=0)+sum(stateD!=0))
      
      # Summarizing number ind in each reproductive state
      sumpopD<-sum(stateD>0)
      sumpopC<-sum(stateC>0)
      sumpopB<-sum(stateB>0)
      # No sumR or stateS of initial state if burn-in
      # No startC if this loop runs once
      }
  
  # Preset to no wasp for burn-in
  stateS<-TRUE
  # For next loop sumpopD ends stateB and update stateC
  #startC<-ifelse(sumpopD>0 & stateB>0,TRUE,FALSE)    
  
  # Restart if needed
  storestateB<-stateB
  storestateC<-stateC
  storestateD<-stateD
  storesumpopB<-sumpopB
  storesumpopC<-sumpopC
  storesumpopD<-sumpopD
  storestartC<-startC
 
  # Prepare burn-in
  while(stateS==TRUE){
    
  # Restart if needed
    stateB<-storestateB
    stateC<-storestateC
    stateD<-storestateD
    sumpopB<-storesumpopB
    sumpopC<-storesumpopC
    sumpopD<-storesumpopD
    startC<-storestartC
 
  burnincounter<-1
  for(burnincounter in as.numeric(0:burnin)){
    initialR <- rbinom(psize,1,4.03/365.25)
    initialR
    
    # Much simplified as indv can only enter reprod via phase B
    # Leaving nowR problem to be dealt with later when writing to state_
    #sampleB <- rtruncnorm(psize,a=0,b=Inf,mean=1.43,sd=0.772)
    sampleB <- rtruncnorm(psize,a=0,b=Inf,mean=10.01,sd=5.404)
    # Check no conflicting phases
    notB <- (initialR==0|sampleB<=0|stateC>0|stateD>0)
    holdstateB <- ifelse(notB, 0, sampleB)
    # holdstate_ for updating phase durations
    #sampleC <- rtruncnorm(psize,a=0,b=Inf,mean=2.88,sd=0.795)
    sampleC <- rtruncnorm(psize,a=0,b=Inf,mean=20.16,sd=5.565)
    holdstateC<-ifelse(sampleC<=0, 0, sampleC)
    #sampleD <- rtruncnorm(psize,a=0,b=Inf,mean=1.22,sd=0.832)
    #add one day for pollinator viability
    #need to add as affecting pollination success
    sampleD <- rtruncnorm(psize,a=0,b=Inf,mean=9.54,sd=5.824)
    holdstateD <-ifelse(sampleD<=0, 0, sampleD)
    
    # Update state_
    stateD<-ifelse(stateD>1, stateD-1, 0)
    endingC<-ifelse(stateC>0&stateC<1,TRUE,FALSE)
    stateD<-ifelse(endingC, holdstateD, stateD)
    # Update alternative male counter with new stateD
    # stateDs<-ifelse(endingC,holdstateD,stateDs)
    # Check if it works; 1>C>0 set to 0
    stateC<-ifelse(stateC>1, stateC-1, 0)
    # sumpopD ends stateB and update stateC
    startC<-ifelse(sumpopD>0 & stateB>0,TRUE,FALSE)
    stateC<-ifelse(startC, holdstateC, stateC)
    # No sumpopD then update stateB
    extendB<-ifelse(sumpopD==0&stateB>1,TRUE,FALSE)
    stateB<-ifelse(extendB, stateB-1, 0)
    # Update with holdstateB
    stateB<-ifelse(holdstateB>0, holdstateB, stateB)
    # remove overlap B with C and D
    removeB<-ifelse(stateC>0|stateD>0,TRUE,FALSE)
    stateB<-ifelse(removeB, 0, stateB)
    
    # Summarizing ind in Reprod
    # number of reproductives assuming that each ind can occupy one state
    sumR<-(sum(stateB!=0)+sum(stateC!=0)+sum(stateD!=0))
    
    # Summarizing number ind in each reproductive state
    sumpopD<-sum(stateD>0)
    sumpopC<-sum(stateC>0)
    sumpopB<-sum(stateB>0)
    
    # No sumR as in burn-in    
    burnincounter<-burnincounter+1
    # Returns stateS TRUE when no wasp
    stateS<-(sumpopC==0&sumpopD==0)
    }
  }
  
  # For next loop sumpopD ends stateB and update stateC
  startC<-ifelse(sumpopD>0 & stateB>0,TRUE,FALSE)
  # try
  stateDs<-stateD
  # Running for 1000 years if wasps persist
  counter<-1
  while (stateS==FALSE&counter<=365250){
    initialR <- rbinom(psize,1,4.03/365.25)
    initialR
    # Much simplified as indv can only enter reprod via phase B
    # Leaving nowR problem to be dealt with later when writing to state_
    #sampleB <- rtruncnorm(psize,a=0,b=Inf,mean=1.43,sd=0.772)
    sampleB <- rtruncnorm(psize,a=0,b=Inf,mean=10.01,sd=5.404)
    # Check no conflicting phases
    notB <- (initialR==0|sampleB<=0|stateC>0|stateD>0)
    holdstateB <- ifelse(notB, 0, sampleB)
    # holdstate_ for updating phase durations
    #sampleC <- rtruncnorm(psize,a=0,b=Inf,mean=2.88,sd=0.795)
    sampleC <- rtruncnorm(psize,a=0,b=Inf,mean=20.16,sd=5.565)
    holdstateC<-ifelse(sampleC<=0, 0, sampleC)
    #sampleD <- rtruncnorm(psize,a=0,b=Inf,mean=1.22,sd=0.832)
    sampleD <- rtruncnorm(psize,a=0,b=Inf,mean=9.54,sd=5.824)
    holdstateD <-ifelse(sampleD<=0, 0, sampleD)

    # New counter for female failure
    endingB <- ifelse(stateB<1 & stateB>0,TRUE,FALSE)
    failedpol <- ifelse(sumpopD==0&endingB,TRUE,FALSE)
    failureF <- ifelse(failedpol,1,0)
    
    # Alternative male success failure
    successDs<-ifelse(sumpopB>0&stateDs>0,1,0)
    endingDs<-ifelse(stateDs>0&stateDs<1,TRUE,FALSE)
    failureDs<-ifelse(endingDs&sumpopB==0,1,0)
    
    # Update alternative male success failure
    stateDs<-ifelse(sumpopB>0&stateDs>0,0,stateDs-1)
    stateDs<-ifelse(stateDs<=0,0,stateDs)
    
    # Update state_
    stateD<-ifelse(stateD>1, stateD-1, 0)
    endingC<-ifelse(stateC>0&stateC<1,TRUE, FALSE)
    stateD<-ifelse(endingC, holdstateD, stateD)
    # Update alternative male counter with new stateD
    stateDs<-ifelse(endingC,holdstateD,stateDs)
    # Check if it works; 1>C>0 set to 0
    stateC<-ifelse(stateC>1, stateC-1, 0)
    # sumpopD ends stateB and update stateC
    startC<-ifelse(sumpopD>0 & stateB>0,TRUE,FALSE) 
    stateC<-ifelse(startC, holdstateC, stateC)
    # No sumpopD then update stateB
    extendB<-ifelse(sumpopD==0&stateB>1,TRUE,FALSE)
    stateB<-ifelse(extendB, stateB-1, 0)
    # Update with holdstateB
    stateB<-ifelse(holdstateB>0, holdstateB, stateB)
    # remove overlap B with C and D
    removeB<-ifelse(stateC>0|stateD>0,TRUE,FALSE)
    stateB<-ifelse(removeB, 0, stateB)
   
    # Summarizing ind in Reprod
    # Number of reproductives assuming that each ind can occupy one state
    sumR<-(sum(stateB!=0)+sum(stateC!=0)+sum(stateD!=0))
   
    # Summarizing number ind in each reproductive state
    sumpopD<-sum(stateD>0)
    sumpopC<-sum(stateC>0)
    sumpopB<-sum(stateB>0)
    
    # New counter for female success using present startC
    successF <- ifelse(startC, 1, 0)
    
    # New temp male tracker using present sumpopB state
    succpolr <- ifelse(stateD>0&sumpopB>0,TRUE,FALSE)
    failpolr <- ifelse(stateD>0&sumpopB==0,TRUE,FALSE)
    successM <- ifelse(succpolr,1,0)
    failureM <- ifelse(failpolr,1,0)
    
    # New summarizing of female male success failure
    sumpopsuccessF<-sum(successF>0)
    sumpopfailureF<-sum(failureF>0)
    sumpopsuccessM<-sum(successM>0)
    sumpopfailureM<-sum(failureM>0)
    sumpopsuccessDs<-sum(successDs>0)
    sumpopfailureDs<-sum(failureDs>0)
    
    # Recording phases after start of burn-in
    countsumpopD<-append(countsumpopD, sumpopD)
    countsumpopC<-append(countsumpopC, sumpopC)
    countsumpopB<-append(countsumpopB, sumpopB)
    countsumpopR<-append(countsumpopR, sumR)
    
    # New countsum_
    countsumpopsuccessF<-append(countsumpopsuccessF, sumpopsuccessF)
    countsumpopfailureF<-append(countsumpopfailureF, sumpopfailureF)
    countsumpopsuccessDs<-append(countsumpopsuccessDs, sumpopsuccessDs)
    countsumpopfailureDs<-append(countsumpopfailureDs, sumpopfailureDs)
    countsumpopsuccessM<-append(countsumpopsuccessM, sumpopsuccessM)
    countsumpopfailureM<-append(countsumpopfailureM, sumpopfailureM)

    counter<-counter+1
    # Returns stateS TRUE when no wasp
    stateS<-(sumpopC==0&sumpopD==0)
    }
  
  # New recording phases
  trackstatesum<- data.frame(countsumpopB, countsumpopC, countsumpopD, countsumpopR,
                             countsumpopsuccessF, countsumpopfailureF, 
                             countsumpopsuccessDs, countsumpopfailureDs,
                             countsumpopsuccessM, countsumpopfailureM)
 
  write.csv(trackstatesum, file = paste0(b_filename, 
                                         "_",
                                         "pop",
                                         psize,
                                         "burnin",
                                         burnin,
                                         "iter",
                                         iteration,
                                         ".csv"), row.names=TRUE)
  
  iteration<-iteration+1  
  
}


  print(paste0( iteration-1," iterations completed for population size of ",psize, " with ", "burn-in of ", burnin ))
}

