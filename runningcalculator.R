rm(list = ls()) 

a = readline(prompt = "Enter starting number")
a = as.integer(a)

n = 1
sum = a
v = NULL    
v = c(v,a)
v = as.integer(v)
df = data.frame(
  num = a,
  freq = 1
)

getmean = function(b) {
  sum <<- sum + b
  n <<- n + 1
  val = sum/n
  return(val)
}

getmode = function(b) {
  found = FALSE
  for (i in 1:length(df$num))
  {
    if (df$num[i] == b) {
      df$freq[i] <<- df$freq[i] + 1
      found = TRUE
      break
    }
  }
  if (!found) {
    df <<- rbind(df,list(b,1))    
  }

  modeval = df$num[1]
  modefreq = df$freq[1]
  for (i in 1:length(df$num))
  {
    if (df$freq[i] > modefreq) {
      modefreq = df$freq[i]
      modeval = df$num[i]
    }
  }
  return(modeval)
}

getmedian = function(b) {
  v <<- c(v,b)   
  v <<- as.integer(v)   
  n <<- n+1
  v <<- sort(v)
  print(v)
  return(v[as.integer(n/2) + 1])
}

opt = 0

getstat = function(b) {
  if (opt == 1)
    return(getmean(b))
  if (opt == 2)
    return(getmode(b))
  if (opt == 3)
    return(getmedian(b))
}

while (TRUE) {
  if (opt == 0)
  {
    opt = readline(prompt = "Enter option (1 for mean, 2 for mode, 3 for median)")
    opt = as.integer(opt)
  }
  b = readline("Enter number")
  b = as.integer(b)
  op = readline(prompt = "Enter operator")
  op = as.character(op)
  if (op == '+')
  {
    a = a+b;
    m = getstat(b)
    print(paste("New stat: ", m))
  }
  else if (op == '-')
  {
    a = a-b;
    m = getstat(b)
    print(paste("New stat: ", m))
  }
  else if (op == '*')
  {
    a = a*b;
    m = getstat(b)
    print(paste("New stat: ", m))
  }
  else if (op == '/')
  {
    a = a/b;
    m = getstat(b)
    print(paste("New stat: ", m))
  }
  else if (op == '^')
  {
    a = a^b;
    m = getstat(b)
    print(paste("New stat: ", m))
  }
  else
  {
    next
  }
  print(paste("New calc. val: ", a))
}