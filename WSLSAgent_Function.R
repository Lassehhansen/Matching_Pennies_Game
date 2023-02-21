WSLSAgent_Function <- function(prevChoice, Feedback) {
  if (Feedback == 1) {
    choice <- prevChoice
  } 
  else if (Feedback == 0) {
    choice <- 1 - prevChoice
  }
  return(choice)} 