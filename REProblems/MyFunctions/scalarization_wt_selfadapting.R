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
  
  #Z = normalize_points(Z, min(Z), max(Z))
  
  return(as.numeric(Z))
  
}