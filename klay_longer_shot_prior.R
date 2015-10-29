## Create vector of Klay's prior shot, for use in analyzing whether the result
## of one shot influences the next shot. This function will be applied to longer
## shots (i.e. eliminating 'short' ones). It considers two factors in 
## determining the prior shot. (1) Is it the start of a new game? If so, the
## prior shot is NA. (2) Is it the first longer shot after a short shot? If so,
## the prior shot is NA.

klay_prior_shot <- function(game_id, shot_no, shot_history) {
  
n = length(shot_history)
klay_prev_shot <- rep(NA, n)
  
for(i in 2:n) {
    if(game_id[i] != game_id[i-1]) {
        klay_prev_shot[i] <- NA
    } else if(shot_no[i] - shot_no[i-1] != 1) {
        klay_prev_shot[i] <- NA
    } else {
        klay_prev_shot[i] <- shot_history[i-1]
    }
  }
return(klay_prev_shot)
}