## Calculate the streaks of made shots per game.
## Runs the analysis on a per game basis, not overlapping different games.
## I'm sure there are more efficient ways of coding this, but as someone 
## relatively new to R programming (Oct. 2015), it works.

streaks <- function(game,shot_made) {
  streak_vector <- c()
  game <- c(head(game,1), game, tail(game,1))
  shot_made <- c(0, shot_made, 0)
  n <- 1
  
  for(i in 2:(length(shot_made)-1)) {
      if(shot_made[i] == 1 && game[i] == game[i-1]) {
          if(game[i+1] == game[i]) {
                if(shot_made[i] == shot_made[i-1] && shot_made[i+1] == 1) {
                    n <- n+1
                } else if(shot_made[i] == shot_made[i-1] && shot_made[i+1] == 0) {
                    streak_vector <- c(streak_vector, n+1)
                    n <- 1
                } else if(shot_made[i+1] == 0 && shot_made[i-1] == 0) {
                    streak_vector <- c(streak_vector, n)
                    n <- 1
                }
          } else if(game[i+1] != game[i]) {
                if(shot_made[i] == shot_made[i-1]) {
                    streak_vector <- c(streak_vector, n+1)
                    n <- 1
                } else {
                    streak_vector <- c(streak_vector, n)
                    n <- 1
                }
          }
      } else if(shot_made[i] == 1 && game[i] != game[i-1]) {
          if(game[i+1] == game[i] && shot_made[i+1] == 1) {
              n <- 1
          } else if(game[i+1] == game[i] && shot_made[i+1] != 1) {
              streak_vector <- c(streak_vector, n)
              n <- 1
          }
      } else if(shot_made == 0 && game[i] != game[i-1]) {
          n <- 1
      }
  }
  return(streak_vector)
}
