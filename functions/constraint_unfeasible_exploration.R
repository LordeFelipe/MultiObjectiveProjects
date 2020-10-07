constraint_unfeasible_exploration <- function(penalties, bigZ, bigV, ...)
{
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    identical(dim(bigZ), dim(bigV)))
  # ==========
  
  e = parent.frame(2)
  total_gen = e$stopcrit[[1]]$maxiter
  current_gen = e$iter
  
  # Determine the parameter based on the current generation
  if(current_gen < floor(total_gen/4)){
    K <- penalties[1]
  }
  else if(current_gen < floor(total_gen/2)){
    K <- penalties[2]
  }
  else{
    K <- penalties[3]
  }
  
  # Calculate penalized values
  bigZV <- bigZ + K * bigV
  
  # Get the selection matrix for all neighborhoods
  sel.indx <- t(apply(bigZV,
                      MARGIN = 2,
                      FUN    = order))
  
  return(sel.indx)
}
