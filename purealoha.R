
rm(list =ls())

fl <- readline(prompt = "Enter the length of a frame in bits")
fl <- as.integer(fl)
br <- readline(prompt = "Enter bitrate of the channel")
br <- as.integer(br)
g <- readline(prompt = "Enter G i.e. mean transmission attempts per frame time")
g <- as.numeric(g)
simtime <- readline(prompt = "Enter total simulation time")
simtime <- as.integer(simtime)


tf <- fl/br        #frame time
lambda <- tf/g     #mean for interarrival time

t <- 0             #curtime
vlnt <- 0          #vulnerable time ends at
tottrans <- 0      #total no. of transmissions attempted
successfultrans <- 0     #total no. of successful transmissions completed

#initial event
t1 <- rexp(1, 1/(2*lambda))       #next arrival time
t = t1
vlnt = t + 2*tf
tottrans = tottrans+1
lastdestroyed = FALSE      #if the current will be destroyed due to the last transmission

arrivals <- t1

while (t < simtime)
{
  t1 = rexp(1, 1/(2*lambda))
  
  arrivals <- c(arrivals,t1)
  
  if ((t+t1) > vlnt && lastdestroyed == FALSE)
  {
    successfultrans = successfultrans + 1       #the last transmission was successful
    t=t+t1
    vlnt = t + 2*tf
    tottrans = tottrans + 1
  }
  else if ((t+t1) > vlnt && lastdestroyed == TRUE)
  {                                                    #start afresh
    t=t+t1
    vlnt = t + 2*tf
    tottrans = tottrans + 1
    lastdestroyed = FALSE
  }
  else
  {
    t=t+t1
    vlnt = t + 2*tf
    tottrans = tottrans + 1
    lastdestroyed = TRUE      #the last transmission + this transmission will get destroyed
  }
}

if (lastdestroyed == FALSE)
  successfultrans = successfultrans + 1

p0 = successfultrans/tottrans
cat("P0 (fraction of attempted frames transmitted successfuly) = ",p0,"\n")
cat("\nBy simulation,\n")
s = 2*successfultrans*(tf/simtime)
cat("S (throughput per frame time) = ",s,"\n")
cat("By theoretical formula,\n")
s = g*p0
cat("S (throughput per frame time) = ",s,"\n")
cat("\nMaximum throughput, S = ",g*exp(-2*g),"\n")
