constraint_multistaged <- function(beta, bigZ, bigV, ...)
{
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    identical(dim(bigZ), dim(bigV)))
  # ==========
  
  # Calculate the 4 multi-staged penalties
  K <- c(beta,beta*2,beta*4,beta*8)
  
  # Calculate penalized values in 4 equal intervals
  bigZV <- matrix(0,ncol=ncol(bigV),nrow = nrow(bigV))
  bigZV[which(bigV < 0.25)] = bigV[which(bigV < 0.25)]*K[1] + bigZ[which(bigV < 0.25)]
  bigZV[which(bigV >= 0.25 & bigV < 0.5)] = bigV[which(bigV >= 0.25 & bigV < 0.5)]*K[2] + bigZ[which(bigV >= 0.25 & bigV < 0.5)]
  bigZV[which(bigV >= 0.5 & bigV < 0.75)] = bigV[which(bigV >= 0.5 & bigV < 0.75)]*K[3] + bigZ[which(bigV >= 0.5 & bigV < 0.75)]
  bigZV[which(bigV >= 0.75)] = bigV[which(bigV >= 0.75)]*K[4] + bigZ[which(bigV >= 0.75)]
  
  # Get the selection matrix for all neighborhoods
  sel.indx <- t(apply(bigZV,
                      MARGIN = 2,
                      FUN    = order))
  
  return(sel.indx)
}