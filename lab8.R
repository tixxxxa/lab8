#1
prime <- function(n){
  if (n%%1 != 0 | n<0){
    return(FALSE)
  }
  else if (n == 1){
    return(FALSE)
  }
  else if (n==2){
    return (TRUE)
  }
  else {
    for (i in 2: (sqrt(n))){
      if(n %% i == 0){
        return(FALSE)
      }
    }
    return(TRUE)
  }
}
prime(13)
prime(0)
prime(20)
prime(-1)

#2
find_runs <- function(x,k){
  n = length(x)
  runs = NULL
  for (i in 1:(n-k+1)){
    if (all (x[i:i+k-1] == 1))
      runs = c(runs, i)
  }
  return (runs)
}

find_runs(c(1, 0, 0, 1, 1, 0, 1, 1, 1), 2)

#3
sort_vec<-function (x){
  if(length(x) <2){
    return (x)
  }
  else{
    for (last in length(x):2){
      for (first in 1:(last -1)){
        if (x[first]> x[first+1]){
          temp <-x[first]
          x[first] = x[first +1]
          x[first +1] = temp
        }
      }
    }
  }
  return (x)
}

sort_vec(c(1,2,3))
sort_vec(c(3,2,3))
sort_vec(c(1))
sort_vec(c(4,7,3,11,34,2,1))
