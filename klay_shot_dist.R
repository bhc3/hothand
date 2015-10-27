## Categorize Klay's shots as long (> 23.7), long mid-range (16-23.7), 
## short mid-range (9-16), short (< 9).

shot_range_f <- function(x) {
  shot_range <- c()

  for(i in 1:length(x)) {
      if(x[i] >= 23.7) {
        shot_range[i] <- "long"
      } else if(x[i] >= 16) {
        shot_range[i] <- "midlong"
      } else if(x[i] >= 9) {
        shot_range[i] <- "midshort"
      } else if(x[i] >= 0) {
        shot_range[i] <- "short"
      } else {
        shot_range[i] <- NA
      }
    }
  return(shot_range)
}