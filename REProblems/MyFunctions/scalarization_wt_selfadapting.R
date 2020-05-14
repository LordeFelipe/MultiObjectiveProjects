scalarization_wt_selfadapting <- function(Y, W, minP, eps = 1e-16, ...){
  
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.matrix(Y) && is.matrix(W),
    identical(dim(W), dim(Y)),
    length(minP) == ncol(Y))
  # ==========
  
  # Replicate minP for dimensional consistency
  minP <- matrix(minP,
                 nrow  = nrow(Y),
                 ncol  = ncol(Y),
                 byrow = TRUE)
  
  Z <- apply(W * (Y - minP + eps),
             MARGIN = 1,
             FUN    = max)
  
  # Calculate the number of feasible solutions in the incubent solution
  rf = sum(parent.frame(2)$Vt$v == 0)/(length(bigV))
  
  if(rf == 0){
    Z = bigV
  }
  else{
    Z = sqrt(bigV^2 + Z^2)
  }
  
  return(as.numeric(Z))
  
}

