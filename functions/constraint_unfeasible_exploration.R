constraint_unfeasible_exploration <- function(penalties, bigZ, bigV, ...)
{
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    identical(dim(bigZ), dim(bigV)))
  # ==========
  
  # Determine the parameter based on the current generation
  if(parent.frame(2)$iter < 65){
    K <- penalties[1]
  }
  else if(parent.frame(2)$iter < 130){
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