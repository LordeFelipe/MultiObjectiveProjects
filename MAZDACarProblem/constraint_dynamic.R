constraint_dynamic <- function(C, alpha, bigZ, bigV, ...)
{
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    identical(dim(bigZ), dim(bigV)))
  # ==========
  
  # Calculate dynamic parameter
  K <- (C*parent.frame(2)$iter)^alpha
  
  # Calculate penalized values
  bigZV <- bigZ + K * bigV
  
  # Get the selection matrix for all neighborhoods
  sel.indx <- t(apply(bigZV,
                      MARGIN = 2,
                      FUN    = order))
  
  return(sel.indx)
}