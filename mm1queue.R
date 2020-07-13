#sample input => simulation time = 1000, lambda=2, mu=3

lambda <- readline(prompt = "Enter mean arrival time")
lambda <- as.integer(lambda)
mu <- readline(prompt = "Enter mean service time")
mu <- as.integer(mu)
simtime <- readline(prompt = "Enter total simulation time")
simtime <- as.integer(simtime)


qlen <- 0                    #just for plotting
eventtime <- 0               #just for plotting
numofevents <- 0             #just for plotting

t <- 0                      #curtime
numofjobs <- 0              #total num of jobs
curjobs <- 0                #current num of jobs
lastdepart <- 0             #index of last departed job

s <- 0                       #qlength*dt
w <- 0                       #sigma(startofservicetime - arrivaltime)
d <- 0                       #sigma(departuretime - arrivaltime)

arrival <- 0                #arrivaltime queue
servicestart <- 0           #startofservicetime queue


#initial event
t1 <- rexp(1,rate = lambda)          #(no.of observations,rate)
t = t1;
numofjobs = numofjobs+1
arrival[numofjobs] = t;
servicestart[numofjobs] = t
curjobs = curjobs+1

numofevents = numofevents+1
eventtime[numofevents] = t
qlen[numofevents] = curjobs


while (t < simtime)
{
  if (curjobs > 0)               #either arrive or depart
  {
    t1 = rexp(1,rate = lambda+mu)
    p = runif(1,0,1)             #(no.of observations,min,max); generate random num b/w min & max
    if (p < lambda/(lambda+mu))  #arrival
    {
      t = t+t1
      numofjobs = numofjobs+1
      arrival[numofjobs] = t
      s = s+curjobs*t1
      curjobs = curjobs+1
    }
    else                         #departure
    {
      t = t+t1
      lastdepart = lastdepart+1
      w = w+servicestart[lastdepart]-arrival[lastdepart]
      d = d+t-arrival[lastdepart]
      if (curjobs != 1)
        servicestart[lastdepart+1] = t
      s = s+curjobs*t1
      curjobs = curjobs-1
    }
  }
  else                     #only arrive
  {
    t1 = rexp(1,lambda)
    t = t+t1
    numofjobs = numofjobs+1
    arrival[numofjobs] = t
    servicestart[numofjobs] = t    
    s = s+curjobs*t1
    curjobs = curjobs+1
  }
  
  numofevents = numofevents+1
  eventtime[numofevents] = t
  qlen[numofevents] = curjobs
}

completedjobs = numofjobs - curjobs
w = w/completedjobs
d = d/completedjobs
s = s/t

cat("Values by Actual Simulation,","\n")
cat("Average Job Waiting Time = ", w, "\n")
cat("Average Job Delay Time = ", d, "\n")
cat("Average Number of Jobs Waiting = ", s, "\n")

cat("Values by Queueing System Formula,","\n")
cat("Average Job Waiting Time = ", lambda/(mu*(mu-lambda)), "\n")
cat("Average Job Delay Time = ", 1/(mu-lambda), "\n")
cat("Average Number of Jobs Waiting = ", lambda/(mu-lambda), "\n")

plot(eventtime, qlen, type = "s", xlab = "Time", ylab = "Queue Length", main = "M/M/1 Simulation")

