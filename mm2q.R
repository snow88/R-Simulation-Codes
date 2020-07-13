
rm(list = ls())

ar <- NULL
svst <- NULL
dep <- NULL
srvrnum <- vector(mode = "numeric", length=100)

busy1 = FALSE
busy2 = FALSE

st <- as.integer(readline(prompt = "simtime"))
lb <- as.integer(readline(prompt = "lambda"))
m <- as.integer(readline(prompt = "mu"))

t = 0
i = 0
lstdep = 0
qlen = 0

while (t<st)
{
  if (qlen == 0)
  {
    t1 = rexp(1,rate=lb)
    i = i+1
    ar[i] = t+t1
    qlen = 1
    svst[i] = t+t1
    busy1 = TRUE
    srvrnum[i] = 1
    t = t+t1
  }
  
  else
  {
    t1 = rexp(1,rate=lb+m)
    rn = runif(1,0,1)
    p = lb/(m+lb)
    if (rn<p)  #arrival
    {
      i = i+1
      ar[i] = t+t1
      qlen = qlen + 1
      if (!busy1) {
        svst[i] = t+t1
        srvrnum[i] = 1
        busy1 = TRUE
      }
      else if (!busy2) {
        svst[i] = t+t1
        srvrnum[i] = 2
        busy2 = TRUE
      }
      t = t+t1
    }
    else  #departure
    {
        lstdep = lstdep+1
        dep[lstdep]=t+t1
        if (srvrnum[lstdep] == 1)
        {
          busy1 = FALSE
          if (lstdep<i && srvrnum[lstdep+1]==0)
          {
              busy1 = TRUE
              srvrnum[lstdep+1]=1
              svst[lstdep+1]=t+t1
          }
          else if (lstdep+1<i)
          {
            busy1 = TRUE
            srvrnum[lstdep+2]=1
            svst[lstdep+2]=t+t1
          }
        }
        else if (srvrnum[lstdep] == 2)
        {
          busy2 = FALSE
          if (lstdep<i && srvrnum[lstdep+1]==0)
          {
            busy2 = TRUE
            srvrnum[lstdep+1]=2
            svst[lstdep+1]=t+t1
          }
          else if (lstdep+1<i)
          {
            busy2 = TRUE
            srvrnum[lstdep+2]=2
            svst[lstdep+2]=t+t1
          }
        }
        t=t+t1
    }
  }
}




