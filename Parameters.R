# This function contains all of the parameters in one location so that it is easy to 
# update the model as needed
LoadParameters<-function()
{
Parameters = data.frame(matrix(vector(), 1, 16, dimnames=list(c(), c("seed", "cols","rows","percpop","biasYN","bias","percgroup","vulnamount",      "daylength","traveltime","detaintime","alpha","gamma","epsilon","pop","popdensity"))),stringsAsFactors=F)
Parameters$seed=7013         # Seed 
Parameters$biasYN=0          # Social Bias: {0=No,1=Yes}
Parameters$bias=.1          # Amount Bias
Parameters$perctype=.3     # Percentage of Biased Group
Parameters$perctarget=.3     # Percentage of Biased Group
Parameters$indirect1=.25
Parameters$indirect2=.75
#Parameters$vulnamount=.05    # Vulnerability
#Parameters$perccrim=.1      # Possibly Increase in Criminal due to Vulnerability  
#Parameters$percsusp=.25    # Possibly Increase in Suspicious due to Vulnerability
Parameters$daylength=10       # Length of Day
Parameters$traveltime=1     # Travel Time
Parameters$detaintime=3      # Detain Time
Parameters$alpha = .6       # Learning Rate [0,1]
Parameters$gamma = .8       # Thoughtfulness Factor [0,1]
Parameters$epsilon = .2     # Exploration Parameter [0,1]
Parameters$N=1000            # Number of Iterations of Sample States
Parameters$MoveReward=0

#Parameters$pop=Parameters$rows*Parameters$cols     # Total Population: pop
Parameters$pop=999
Parameters$popdensity=Parameters$percpop*Parameters$cols*Parameters$rows  #Density of population

return(Parameters)
}