## This function will partition a vector of binomial values (e.g. made_miss)
## into groups of 4. It will then count the number of made shots (each made
## shot has a value of 1) in each unique 4-game set.
## The function also ensures the vector is a multiple of 4 by clipping the 
## end of the vector that exceeds a multiple of 4.

klay_quad <- function(x) {
  n <- length(x) - length(x)%%4
  klay_quad_perf <- rep(NA, n/4)
  count <- 1
  
  for(i in seq(1,n,4)) {
      quad <- x[i] + x[i+1] + x[i+2] + x[i+3]
      klay_quad_perf[count] <- quad
      count <- count+1
  }
  return(klay_quad_perf)
}