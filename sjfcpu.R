
rm(list = ls())

df <- read.csv("sjfcpu.csv")
df <- df[order(df$ArrivalTime),]
attach(df)

t = 1

i = 1
q <- NULL
servend <- NULL

busy = FALSE

while (t < 100) {
  if (i <= length(ArrivalTime) && t == ArrivalTime[i]) {
    q = c(q, i)
    i=i+1
  }
  
  if (!busy && length(q)>0)
  {
    mincomp = 9999
    minj = 0
    for (j in 1:length(q))
    {
      if (CompletionTime[q[j]] < mincomp)
      {
        mincomp = CompletionTime[q[j]]
        minj = j
      }
    }
    busy = TRUE
    servend[q[minj]] = t + CompletionTime[q[minj]]
    q <- q[-minj]
  }
  
  for (v in 1:length(servend))
  {
    if (!is.na(servend[v]) && servend[v] == t) {
      busy = FALSE
    }
  }

 t = t+1 
 print(q)
 print(servend)
}

detach(df)
