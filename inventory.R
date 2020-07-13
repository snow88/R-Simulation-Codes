
rm(list=ls())

simtime = 100
lambda = 3
mu = 2

x = 10     #initial supply
newsupply = 50
q <- NULL
completion <- NULL
demands <- NULL
lstdep = 0
i=0
t=0
curjobs = 0

while (t<100)
{
  r = runif(1,0,1)
  
  if (r<0.5)
  {
    #supply
    t1 = rexp(1,lambda)
    if (length(q) > 0) {
      x = x+newsupply
      t=t+t1
    }
  }
  else
  {
    #demand
    if(curjobs == 0)
    {
      t1 = rexp(1,lambda)
      i = i+1
      q[i] = t+t1
      demands[i] = as.integer(runif(1,0,100))
      if (x >= demands[i]) {
        x = x - demands[i]
        completion[i] = t+t1
        lstdep = lstdep+1
      }
      else
        curjobs = curjobs + 1
      t=t+t1
    }
    else
    {
      t1 = rexp(1,lambda+mu)
      p = runif(1,0,1)
      if (p < lambda/(mu+lambda))
      {
        i = i+1
        q[i] = t+t1
        demands[i] = as.integer(runif(1,0,100))
        curjobs = curjobs+1
        t=t+t1
      }
      else
      {
        if (demands[lstdep+1] <= x) {
          lstdep = lstdep+1
          completion[lstdep] = t+t1
          x = x - demands[lstdep]
          curjobs = curjobs-1
        }
        t=t+t1
      }
    }
  }
}