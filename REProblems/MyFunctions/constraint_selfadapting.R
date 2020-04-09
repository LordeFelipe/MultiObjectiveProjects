constraint_selfadapting <- function(bigZ, bigV, ...)
{
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    identical(dim(bigZ), dim(bigV)))
  # ==========
  
  # Calculate the number of feasible solutions in the incubent solution
  rf = sum(parent.frame(2)$Vt$v == 0)
  
  # Get the feasible solutions of the new generation
  feasibilityMatrix = parent.frame(2)$V$v == 0
  
  #Calculate M(X)
  if(rf == 0){
    M = matrix(0,nrow = nrow(bigV), ncol = ncol(bigV))
  }
  else{
    M = bigV
  }
  
  #Calculate N(X)
  N = matrix(0,nrow = nrow(bigV), ncol = ncol(bigV))
  for(i in 1:ncol(bigZ)){
    if(feasibilityMatrix[i] != 0){
      N[,i] = bigZ[,i]
    }
  }
  
  # Calculate p(X)
  P = (1-rf)*M + rf*N
  
  # Calculate penalized values
  bigZV <- bigZ + P
  
  # Get the selection matrix for all neighborhoods
  sel.indx <- t(apply(bigZV,
                      MARGIN = 2,
                      FUN    = order))
  
  return(sel.indx)
}