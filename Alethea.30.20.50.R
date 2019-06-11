
##########################
##
##########################

#' Computes the reinforcement learning policy
#'
#' Computes reinforcement learning policy from a given state-action table Q.
#' The policy is the decision-making function of the agent and defines the learning
#' agent's behavior at a given time.
#'
#' @param x Variable which encodes the behavior of the agent. This can be
#' either a \code{matrix}, \code{data.frame} or an \code{\link{rl}} object.
#' @seealso \code{\link{ReinforcementLearning}}
#' @return Returns the learned policy.
#' @examples
#' # Create exemplary state-action table (Q) with 2 actions and 3 states
#' Q <- data.frame("up" = c(-1, 0, 1), "down" = c(-1, 1, 0))
#'
#' # Show best possible action in each state
#' computePolicy(Q)
#'
#' @rdname computePolicy
#' @export
computePolicy <- function(x) {
  UseMethod("computePolicy", x)
}

#' @export
computePolicy.matrix <- function(x) {
  policy <- colnames(x)[apply(x, 1, which.max)]
  names(policy) <- rownames(x)
  return(policy)
}

#' @export
computePolicy.data.frame <- function(x) {
  return(computePolicy(as.matrix(x)))
}

#' @export
computePolicy.rl <- function(x) {
  return(computePolicy(x$Q))
}

#' @export
computePolicy.default <- function(x) {
  stop("Argument invalid.")
}

#' Computes the reinforcement learning policy
#'
#' Deprecated. Please use [ReinforcementLearning::computePolicy()] instead.
#'
#' @param x Variable which encodes the behavior of the agent. This can be
#' either a \code{matrix}, \code{data.frame} or an \code{\link{rl}} object.
#' @seealso \code{\link{ReinforcementLearning}}
#' @return Returns the learned policy.
#' @rdname policy
#' @export
policy <- function(x) {
  .Deprecated("computePolicy")
  computePolicy(x)
}


###################################################################
## This function contains all of the parameters in one location  ##
## so that it is easy to update the model as needed              ##  
###################################################################


LoadParameters<-function()
{
  parameters = data.frame(matrix(vector(), 1, 16, dimnames=list(c(), c("seed", "cols","rows","percpop","biasYN","bias","percgroup","vulnamount",      "daylength","traveltime","detaintime","alpha","gamma","epsilon","pop","popdensity"))),stringsAsFactors=F)
  parameters$seed=7013         # Seed 
  parameters$bias=.1          # Amount Bias
  parameters$percGroup=.4     # Percentage of Biased Group
  parameters$indirect1=.4
  parameters$indirect2=.6
  #parameters$vulnamount=.05    # Vulnerability
  #parameters$perccrim=.1      # Possibly Increase in Criminal due to Vulnerability  
  #parameters$percsusp=.25    # Possibly Increase in Suspicious due to Vulnerability
  parameters$daylength=28     # Length of Day
  parameters$traveltime=1     # Travel Time
  parameters$detaintime=4      # Detain Time
  parameters$alpha = .6       # Learning Rate [0,1]
  parameters$gamma = .8       # Thoughtfulness Factor [0,1]
  parameters$epsilon = .3     # Exploration Parameter [0,1]
  parameters$MoveReward=0
  parameters$numtrain=30 # Number Train runs
  parameters$numtest=20 # Number of Test runs
  parameters$itertrain=50 #Number Train Iterations
  #Parameters$pop=Parameters$rows*Parameters$cols     # Total Population: pop
  parameters$pop=999
  
  return(parameters)
}

###################################################################
##
###################################################################

createsamplefunction<-function(population, parameters) {
  
  id_num0<-sample(1:nrow(population), parameters$daylength*2, replace=F)
  Left<-NA
  Right<-NA
  LeftReward<-NA
  RightReward<-NA
  LeftCrim<-NA
  RightCrim<-NA
  LeftGroup<-NA
  RightGroup<-NA
  LeftSusp<-NA
  RightSusp<-NA
  LeftSuspCode<-NA
  RightSuspCode<-NA
  LeftDirectCode<-NA
  RightDirectCode<-NA
  LeftIndirectCode<-NA
  RightIndirectCode<-NA
  NextSuspState<-NA 
  NextDirectState<-NA
  NextIndirectState<-NA
  SuspState<-NA 
  DirectState<-NA
  IndirectState<-NA
  State<-NA
  NextState<-NA
  for(ii in 1: parameters$daylength)
  {
    id_num1<-id_num0[ii]
    id_num2<-id_num0[ii+parameters$daylength]
    Left[ii]<-id_num1
    Right[ii]<-id_num2
    LeftReward[ii]<-population$Reward[id_num1]
    RightReward[ii]<-population$Reward[id_num2]
    LeftGroup[ii]<-population$Group[id_num1]
    RightGroup[ii]<-population$Group[id_num2]
    LeftSusp[ii]<-population$Susp[id_num1]
    RightSusp[ii]<-population$Susp[id_num2]
    LeftCrim[ii]<-population$Crim[id_num1]
    RightCrim[ii]<-population$Crim[id_num2]
    State[ii]<-NA
    LeftSuspCode[ii]<-population$Susp[id_num1]
    RightSuspCode[ii]<-population$Susp[id_num2]
    LeftDirectCode[ii]<-population$DirectCode[id_num1]
    RightDirectCode[ii]<-population$DirectCode[id_num2]
    LeftIndirectCode[ii]<-population$IndirectCode[id_num1]
    RightIndirectCode[ii]<-population$IndirectCode[id_num2]
    SuspState[ii]<-paste0(LeftSuspCode[ii],'.',RightSuspCode[ii])
    DirectState[ii]<-paste0(LeftDirectCode[ii],'.',RightDirectCode[ii])
    IndirectState[ii]<-paste0(LeftIndirectCode[ii],'.',RightIndirectCode[ii])
    NextSuspState[ii-1]<-paste0(LeftSuspCode[ii],'.',RightSuspCode[ii])
    NextSuspState[ii]<-"End" 
    NextDirectState[ii-1]<-paste0(LeftDirectCode[ii],'.',RightDirectCode[ii])
    NextDirectState[ii]<-"End"
    NextIndirectState[ii-1]<-paste0(LeftIndirectCode[ii],'.',RightIndirectCode[ii])
    NextIndirectState[ii]<-"End"
  }
  createRLsample<-data.frame(Left,LeftGroup, LeftSusp,LeftSuspCode,LeftDirectCode,LeftIndirectCode, LeftCrim, LeftReward, Right, RightGroup, RightSusp, RightSuspCode,   RightDirectCode, RightIndirectCode, RightCrim, RightReward,SuspState, DirectState, IndirectState, NextSuspState, NextDirectState, NextIndirectState)
  return(createRLsample)
}

###################################################################
##
###################################################################

statediagramfunction <- function(createsample, parameters,flag) {
  time = parameters$daylength
  detain = parameters$detaintime
  move = parameters$traveltime
  
  statemap<-data.frame("State"=paste0(time,".",createsample$State[1]), 
                       "Action"="Left", 
                       "Reward"=createsample$LeftReward[1], 
                       "NextState"=paste0(time-detain,".",createsample$NextState[1]),
                       "LeftGroup"=createsample$LeftGroup[1],
                       "LeftSusp"=createsample$LeftSusp[1],
                       "LeftCrim"=createsample$LeftCrim[1],
                       "LeftReward"=createsample$LeftReward[1],
                       "RightGroup"=createsample$RightGroup[1],
                       "RightSusp"=createsample$RightSusp[1],                 
                       "RightCrim"=createsample$RightCrim[1],
                       "RightReward"=createsample$RightReward[1])
  nextrow<-data.frame("State"=paste0(time,".",createsample$State[1]), 
                      "Action"="Right", 
                      "Reward"=createsample$RightReward[1], 
                      "NextState"=paste0(time-detain,".",createsample$NextState[1]),
                      "LeftGroup"=createsample$LeftGroup[1],
                      "LeftSusp"=createsample$LeftSusp[1],
                      "LeftCrim"=createsample$LeftCrim[1],
                      "LeftReward"=createsample$LeftReward[1],
                      "RightGroup"=createsample$RightGroup[1],
                      "RightSusp"=createsample$RightSusp[1],                 
                      "RightCrim"=createsample$RightCrim[1],
                      "RightReward"=createsample$RightReward[1])
  statemap<-rbind(statemap,nextrow)
  nextrow<-data.frame("State"=paste0(time,".",createsample$State[1]), 
                      "Action"="None", 
                      "Reward"=0, 
                      "NextState"=paste0(time-move,".",createsample$NextState[1]),
                      "LeftGroup"=createsample$LeftGroup[1],
                      "LeftSusp"=createsample$LeftSusp[1],
                      "LeftCrim"=createsample$LeftCrim[1],
                      "LeftReward"=createsample$LeftReward[1],
                      "RightGroup"=createsample$RightGroup[1],
                      "RightSusp"=createsample$RightSusp[1],                 
                      "RightCrim"=createsample$RightCrim[1],
                      "RightReward"=createsample$RightReward[1])
  statemap<-rbind(statemap,nextrow)
  
  for(ii in 1:1000)
  {
    statedummy<-as.character(statemap$NextState[ii])
    flag<-0
    if (is.na(statedummy))
    {}
    else if (statedummy=="End")
    {}
    else
    {
      for(jj in 1:nrow(statemap))
      {
        if (statedummy==statemap$State[jj])  
        {
          flag<-1
        }
      }
      if(flag==0)
      {
        openstate<-unlist(stri_split_fixed(as.character(statedummy),".", fixed = TRUE, n=2))
        for (kk in 1:nrow(createsample))
        {
          if (openstate[2]==as.character(createsample$State[kk]))
          {
            timedet<-as.numeric(openstate[1])-detain
            timemove<-as.numeric(openstate[1])-move
            if(as.numeric(openstate[1])>=detain)
            {
              nextrow<-data.frame("State"=statedummy, 
                                  "Action"="Left", 
                                  "Reward"=createsample$LeftReward[kk], 
                                  "NextState"=paste0(timedet,".",createsample$State[kk+1]),
                                  "LeftGroup"=createsample$LeftGroup[kk],
                                  "LeftSusp"=createsample$LeftSusp[kk],
                                  "LeftCrim"=createsample$LeftCrim[kk],
                                  "LeftReward"=createsample$LeftReward[kk],
                                  "RightGroup"=createsample$RightGroup[kk],
                                  "RightSusp"=createsample$RightSusp[kk],                 
                                  "RightCrim"=createsample$RightCrim[kk],
                                  "RightReward"=createsample$RightReward[kk]
              )
              nextrow2<-data.frame("State"=statedummy, 
                                   "Action"="Right", 
                                   "Reward"=createsample$RightReward[kk], 
                                   "NextState"=paste0(timedet,".",createsample$State[kk+1]),
                                   "LeftGroup"=createsample$LeftGroup[kk],
                                   "LeftSusp"=createsample$LeftSusp[kk],
                                   "LeftCrim"=createsample$LeftCrim[kk],
                                   "LeftReward"=createsample$LeftReward[kk],
                                   "RightGroup"=createsample$RightGroup[kk],
                                   "RightSusp"=createsample$RightSusp[kk],                 
                                   "RightCrim"=createsample$RightCrim[kk],
                                   "RightReward"=createsample$RightReward[kk])
            } 
            else if(as.numeric(openstate[1])<detain)
            {
              nextrow<-data.frame("State"=statedummy, 
                                  "Action"="Left", 
                                  "Reward"=-50, 
                                  "NextState"="End",
                                  "LeftGroup"=createsample$LeftGroup[kk],
                                  "LeftSusp"=createsample$LeftSusp[kk],
                                  "LeftCrim"=createsample$LeftCrim[kk],
                                  "LeftReward"=createsample$LeftReward[kk],
                                  "RightGroup"=createsample$RightGroup[kk],
                                  "RightSusp"=createsample$RightSusp[kk],                 
                                  "RightCrim"=createsample$RightCrim[kk],
                                  "RightReward"=createsample$RightReward[kk])
              nextrow2<-data.frame("State"=statedummy, 
                                   "Action"="Right", 
                                   "Reward"=-50, 
                                   "NextState"="End",
                                   "LeftGroup"=createsample$LeftGroup[kk],
                                   "LeftSusp"=createsample$LeftSusp[kk],
                                   "LeftCrim"=createsample$LeftCrim[kk],
                                   "LeftReward"=createsample$LeftReward[kk],
                                   "RightGroup"=createsample$RightGroup[kk],
                                   "RightSusp"=createsample$RightSusp[kk],                 
                                   "RightCrim"=createsample$RightCrim[kk],
                                   "RightReward"=createsample$RightReward[kk])
            } 
            if(as.numeric(openstate[1])>=move)
            {
              nextrow3<-data.frame("State"=statedummy, 
                                   "Action"="None", 
                                   "Reward"=0, 
                                   "NextState"=paste0(timemove,".",createsample$State[kk+1]),
                                   "LeftGroup"=createsample$LeftGroup[kk],
                                   "LeftSusp"=createsample$LeftSusp[kk],
                                   "LeftCrim"=createsample$LeftCrim[kk],
                                   "LeftReward"=createsample$LeftReward[kk],
                                   "RightGroup"=createsample$RightGroup[kk],
                                   "RightSusp"=createsample$RightSusp[kk],                 
                                   "RightCrim"=createsample$RightCrim[kk],
                                   "RightReward"=createsample$RightReward[kk])
              statemap<-rbind(statemap, nextrow, nextrow2, nextrow3)
            }
            else if (as.numeric(openstate[1])<move)
            {
              nextrow3<-data.frame("State"=statedummy, 
                                   "Action"="None", 
                                   "Reward"=0, 
                                   "NextState"="End",
                                   "LeftGroup"=createsample$LeftGroup[kk],
                                   "LeftSusp"=createsample$LeftSusp[kk],
                                   "LeftCrim"=createsample$LeftCrim[kk],
                                   "LeftReward"=createsample$LeftReward[kk],
                                   "RightGroup"=createsample$RightGroup[kk],
                                   "RightSusp"=createsample$RightSusp[kk],                 
                                   "RightCrim"=createsample$RightCrim[kk],
                                   "RightReward"=createsample$RightReward[kk])
              statemap<-rbind(statemap, nextrow, nextrow2, nextrow3)
            }
          } 
        }
      }
    }
  }
  return(statemap)
}

###################################################################
##
###################################################################

runRL<-function(RLdat, trainmodelold, parameters)
{
  # Load dataset
  RLdat$State<-as.character(RLdat$State)
  RLdat$NextState<-as.character(RLdat$NextState)
  RLdat$Action<-as.character(RLdat$Action)
  # Define reinforcement learning parameters
  control <- list(alpha = parameters$alpha, gamma = parameters$gamma, epsilon = parameters$epsilon)
  # Perform reinforcement learning
  trainmodelnew <- ReinforcementLearning(RLdat, s = "State", a = "Action", r = 
                                           "Reward", 
                                         s_new = "NextState", iter = parameters$itertrain, control = control, model=trainmodelold)
  # Print optimal policy
  return(trainmodelnew)
}


###################################################################
##
###################################################################
#This is the initial reinforcement learning routine.

runRLinit<-function(RLdat,parameters)
{
  # Load dataset
  RLdat$State<-as.character(RLdat$State)
  RLdat$NextState<-as.character(RLdat$NextState)
  RLdat$Action<-as.character(RLdat$Action)
  # Define reinforcement learning parameters
  control <- list(alpha = parameters$alpha, gamma = parameters$gamma, epsilon = parameters$epsilon)
  
  # Perform reinforcement learning
  trainmodelnew <- ReinforcementLearning(RLdat, s = "State", a = "Action", r = 
                                           "Reward", 
                                         s_new = "NextState", iter = parameters$itertrain, control = control)
  # Print optimal policy
  return(trainmodelnew)
}



###################################################################
## This is the population seeding routine.
###################################################################



createpopulation<-function(parameters)
{
  set.seed=(parameters$seed)
  
  population = data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("ID", "Group", "Crim", "Indirect","Reward", "Susp", "DirectCode", "IndirectCode"))),stringsAsFactors=F)
  person = data.frame(matrix(vector(), 1, 8, dimnames=list(c(), c("ID", "Group", "Crim", "Indirect","Reward", "Susp", "DirectCode", "IndirectCode"))),stringsAsFactors=F)
  
  for (ii in 1:parameters$pop)
  {
    person$ID <-sprintf("%03d",ii)
    person$Group <- sample(1:2, 1, replace=T,prob=c(1-parameters$percGroup,parameters$percGroup))
    person$Crim<- sample(1:4, 1, replace=T)
    person$Reward=person$Crim
    if (person$Group==1)
    {
      person$Indirect<- sample(0:1, 1, replace=T,prob=c(1-parameters$indirect1,parameters$indirect1))
      person$Susp<-  round(person$Crim +   rnorm(1,0,1))
      if(person$Susp<0)
      {person$Susp<-0}
    }
    if (person$Group==2)
    {
      person$Indirect<- sample(0:1, 1, replace=T,prob=c(1-parameters$indirect2,1-parameters$indirect2))
      person$Susp<-  round(person$Crim +   rnorm(1,1,1))
      if(person$Susp<0)
      {person$Susp<-0}
    }
    person$DirectCode<-paste0(person$Group,'.',person$Susp)
    population<-rbind(population,person)
    person$IndirectCode<-paste0(person$Indirect,'.',person$Susp)
    population<-rbind(population,person)
  }
  return(population)
}


###################################################################
## Function Policywork
###################################################################

policywork<-function(policytest,test)
{
  policytest<- data.frame(unlist(policytest))
  policytest<-cbind(policytest,State=rownames(policytest))
  policytest$State<-as.character(policytest$State)
  finalpolicy<-NA
  for (ii in 1:nrow(policytest))
  {
    policytest$State[ii]<-sub('X','', policytest$State[ii])
  }
  finalpolicy<-data.frame("State"=NA,"Action"=NA, "Reward"=NA, "LeftCrim"=NA, "LeftGroup"=NA, "LeftSusp"=NA, "LeftReward"=NA, "RightCrim"=NA, "RightGroup"=NA, "RightSusp"=NA, "RightReward"=NA, "NextState"=NA)
  finalpolicy2<-finalpolicy
  
  for(jj in 1:nrow(test))
  {
    for (kk in 1:nrow(policytest))
    {
      
      if(policytest$State[kk]==test$State[jj]&&as.character(policytest$unlist.policytest.[kk])==as.character(test$Action[jj]))
      { finalpolicy2$State<-policytest$State[kk]
      finalpolicy2$Action<-policytest$unlist.policytest.[kk]
      finalpolicy2$Reward<-test$Reward[jj]
      finalpolicy2$LeftCrim<-test$LeftCrim[jj]   
      finalpolicy2$LeftGroup<-test$LeftGroup[jj]  
      finalpolicy2$LeftSusp<-test$LeftSusp[jj] 
      finalpolicy2$LeftReward<-test$LeftReward[jj]    
      finalpolicy2$RightCrim<-test$RightCrim[jj] 
      finalpolicy2$RightGroup<-test$RightGroup[jj]  
      finalpolicy2$RightSusp<-test$RightSusp[jj] 
      finalpolicy2$RightReward<-test$RightReward[jj]          
      finalpolicy2$NextState<-test$NextState[jj] 
      finalpolicy<-rbind(finalpolicy, finalpolicy2)
      }
    }
  }
  finalpolicy<-unique(finalpolicy)
  finalpolicy<-na.omit(finalpolicy)
  return(finalpolicy)
}


RLFinalSolution<-function(finalpolicy)
{
  RLsolution<-finalpolicy[1,]
  for(ii in 1:parameters$daylength)
  {
    if(!is.na(RLsolution$NextState[ii]))
    {
      if(RLsolution$NextState[ii]=="End")
      {break}
      for(jj in 1:nrow(finalpolicy))
      {
        if (RLsolution$NextState[ii]==finalpolicy$State[jj])
        {
          RLsolution[ii+1,]<-finalpolicy[jj,]
        }
      }
    }
    for(kk in 1:nrow(RLsolution))
    {if (RLsolution$Action[kk]=="Left")
    {RLsolution$Group[kk]<-RLsolution$LeftGroup[kk]}
      if (RLsolution$Action[kk]=="Right")  
      {RLsolution$Group[kk]<-RLsolution$RightGroup[kk]}
      if (RLsolution$Action[kk]=="None")  
      {RLsolution$Group[kk]<-NA}
    } }
  return(RLsolution)
}

###################################################################
##
###################################################################


library(stringi)
library(dplyr)
library(ReinforcementLearning)
library(binaryLogic)
library(stats)
library(hash)
library(ggplot2)
library(testthat)
library(wesanderson)

#setwd("~/Alethea")

#population <- read.csv("~/Alethea/population.csv")

parameters<-LoadParameters()

population<-createpopulation(parameters)
save(population, file = paste0("Population.",parameters$seed,".rda"))
write.csv(population, file = paste0("Population.",parameters$seed,".csv"),row.names=FALSE)

createsample<-createsamplefunction(population, parameters)
createsampleSusp<-createsample
createsampleDirect<-createsample
createsampleIndirect<-createsample

###################################################################
##
###################################################################

for(i in 1:nrow(createsample))
{
  createsampleSusp$State[i]<-createsample$SuspState[i]
  createsampleDirect$State[i]<-createsample$DirectState[i]
  createsampleIndirect$State[i]<-createsample$IndirectState[i] 
  
  createsampleSusp$NextState[i]<-createsample$SuspNextState[i]
  createsampleDirect$NextState[i]<-createsample$DirectNextState[i]
  createsampleIndirect$NextState[i]<-createsample$IndirectNextState[i]
}

trainSusp<-statediagramfunction(createsampleSusp, parameters)
trainmodelSusp<-runRLinit(trainSusp, parameters)

trainDirect<-statediagramfunction(createsampleDirect, parameters)
trainmodelDirect<-runRLinit(trainDirect, parameters)

trainIndirect<-statediagramfunction(createsampleIndirect, parameters)
trainmodelIndirect<-runRLinit(trainIndirect, parameters)

for (m in 1:parameters$numtrain)
{
  RLtrainsample<-createsamplefunction(population, parameters)
  RLtrainsampleSusp<-RLtrainsample
  RLtrainsampleDirect<-RLtrainsample
  RLtrainsampleIndirect<-RLtrainsample
  for(i in 1:parameters$daylength)
  {
    
    RLtrainsampleSusp$State[i]<-as.character(RLtrainsample$SuspState[i])
    RLtrainsampleDirect$State[i]<-as.character(RLtrainsample$DirectState[i])
    RLtrainsampleIndirect$State[i]<-as.character(RLtrainsample$IndirectState[i]) 
    RLtrainsampleSusp$NextState[i]<-as.character(RLtrainsample$NextSuspState[i])
    RLtrainsampleDirect$NextState[i]<-as.character(RLtrainsample$NextDirectState[i])
    RLtrainsampleIndirect$NextState[i]<-as.character(RLtrainsample$NextIndirectState[i])
  }
  
  trainSusp<-statediagramfunction(RLtrainsampleSusp, parameters)
  trainmodelSusp<-runRL(trainSusp, trainmodelSusp,parameters)
  policytrainSusp<-computePolicy(trainmodelSusp)
  
  trainDirect<-statediagramfunction(RLtrainsampleDirect, parameters)
  trainmodelDirect<-runRL(trainDirect, trainmodelDirect,parameters)
  policytrainDirect<-computePolicy(trainmodelDirect)
  
  trainIndirect<-statediagramfunction(RLtrainsampleIndirect, parameters)
  trainmodelIndirect<-runRL(trainIndirect, trainmodelIndirect,parameters)
  policytrainIndirect<-computePolicy(trainmodelIndirect)
}

###################################################################
##
###################################################################

RLsolutionSusp<-NA
RLsolutionDirect<-NA
RLsolutionIndirect<-NA

for(j in 1:parameters$numtest)
{
  RLtestsample<-createsamplefunction(population, parameters)
  RLtestsampleSusp<-RLtestsample
  RLtestsampleDirect<-RLtestsample
  RLtestsampleIndirect<-RLtestsample
  for(i in 1:nrow(RLtrainsample))
  {
    
    RLtestsampleSusp$State[i]<-as.character(RLtestsample$SuspState[i])
    RLtestsampleDirect$State[i]<-as.character(RLtestsample$DirectState[i])
    RLtestsampleIndirect$State[i]<-as.character(RLtestsample$IndirectState[i]) 
    
    RLtestsampleSusp$NextState[i]<-as.character(RLtestsample$NextSuspState[i])
    RLtestsampleDirect$NextState[i]<-as.character(RLtestsample$NextDirectState[i])
    RLtestsampleIndirect$NextState[i]<-as.character(RLtestsample$NextIndirectState[i])
  }
  
  testSusp<-statediagramfunction(RLtestsampleSusp, parameters)
  testmodelSusp<-runRL(testSusp,trainmodelSusp,parameters)
  
  testDirect<-statediagramfunction(RLtestsampleDirect, parameters)
  testmodelDirect<-runRL(testDirect,trainmodelDirect,parameters)
  
  testIndirect<-statediagramfunction(RLtestsampleIndirect, parameters)
  testmodelIndirect<-runRL(testIndirect,trainmodelIndirect,parameters)
  
  save(testSusp, file = paste0("testSusp.daylength.",parameters$daylength,".numtrain.",parameters$numtrain,".numtest.",parameters$numtest,".itertrain.",parameters$itertrain,".rda"))
  save(testDirect, file = paste0("testDirect.daylength.",parameters$daylength,".numtrain.",parameters$numtrain,".numtest.",parameters$numtest,".itertrain.",parameters$itertrain,".rda"))
  save(testIndirect, file = paste0("testIndirect.daylength.",parameters$daylength,".numtrain.",parameters$numtrain,".numtest.",parameters$numtest,".itertrain.",parameters$itertrain,".rda"))
  
  policytestSusp<-computePolicy(testmodelSusp)
  finalpolicySusp<-policywork(policytestSusp,testSusp)
  
  policytestDirect<-computePolicy(testmodelDirect)
  finalpolicyDirect<-policywork(policytestDirect,testDirect)
  
  policytestIndirect<-computePolicy(testmodelIndirect)
  finalpolicyIndirect<-policywork(policytestIndirect, testIndirect)
  
  RLsolutionSusp2<-RLFinalSolution(finalpolicySusp)
  RLsolutionDirect2<-RLFinalSolution(finalpolicyDirect)
  RLsolutionIndirect2<-RLFinalSolution(finalpolicyIndirect)
  
  RLsolutionSusp<-rbind(RLsolutionSusp,RLsolutionSusp2)
  RLsolutionDirect<-rbind(RLsolutionDirect,RLsolutionDirect2)
  RLsolutionIndirect<-rbind(RLsolutionIndirect,RLsolutionIndirect2)
}

table(RLsolutionSusp$Group)
table(RLsolutionDirect$Group)
table(RLsolutionIndirect$Group)

save(RLsolutionSusp, file = paste0("RLsolutionSusp.daylength.",parameters$daylength,".numtrain.",parameters$numtrain,".numtest.",parameters$numtest,".itertrain.",parameters$itertrain,".rda"))
save(RLsolutionDirect, file = paste0("RLsolutionDirect.daylength.",parameters$daylength,".numtrain.",parameters$numtrain,".numtest.",parameters$numtest,".itertrain.",parameters$itertrain,".rda"))
save(RLsolutionIndirect, file = paste0("RLsolutionIndirect.daylength.",parameters$daylength,".numtrain.",parameters$numtrain,".numtest.",parameters$numtest,".itertrain.",parameters$itertrain,".rda"))

testmodelIdealSusp<-runRLinit(testSusp,parameters)
testmodelIdealDirect<-runRLinit(testDirect,parameters)
testmodelIdealIndirect<-runRLinit(testIndirect,parameters)

policytestIdealSusp<-computePolicy(testmodelIdealSusp)
finalpolicyIdealSusp<-policywork(policytestIdealSusp,testSusp)

policytestIdealDirect<-computePolicy(testmodelIdealDirect)
finalpolicyIdealDirect<-policywork(policytestIdealDirect,testDirect)

policytestIdealIndirect<-computePolicy(testmodelIdealIndirect)
finalpolicyIdealIndirect<-policywork(policytestIdealIndirect, testIndirect)

RLsolutionSuspIdeal<-RLFinalSolution(finalpolicyIdealSusp)
RLsolutionDirectIdeal<-RLFinalSolution(finalpolicyIdealDirect)
RLsolutionIndirectIdeal<-RLFinalSolution(finalpolicyIdealIndirect)

table(RLsolutionSuspIdeal$Group)
table(RLsolutionDirectIdeal$Group)
table(RLsolutionIndirectIdeal$Group)

save(RLsolutionSuspIdeal, file = paste0("RLsolutionSuspIdeal.itertrain.",parameters$itertrain,".rda"))
save(RLsolutionDirectIdeal, file = paste0("RLsolutionDirectIdeal.itertrain.",parameters$itertrain,".rda"))
save(RLsolutionIndirect, file = paste0("RLsolutionIndirectIdeal.itertrain.",parameters$itertrain,".rda"))


