MemAgent_Function <- function(input, msize, noise, power){
  if (power == 0) {
    
    input = input[!is.na(input)]
    input = tail(input, n = msize)
    choice = round(mean(input))
    
    if (rbinom(1, 1, noise) == 1) 
    {choice = rbinom(1, 1, 0.5)}
    
  }
  else if (power > 0){
    
    input = input[!is.na(input)]
    input = tail(input, n = msize)
    weights <- (1:length(input)) ^ (-power)
    weights <- weights / sum(weights)
    choice <- round(sum(msize * weights))
    
    # Add noise to the agent's choice
    if (rbinom(1, 1, noise) == 1) { 
      choice <- rbinom(1, 1, 0.5)
    }
  }
  
  return(choice) 
}