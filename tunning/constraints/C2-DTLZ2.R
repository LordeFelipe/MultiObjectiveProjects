constraint_C2DTLZ2 <- function(X){
  
  Y = DTLZ(X)
  minimum = rep(0, n_variables)
  maximum = rep(1, n_variables)
  n_objs = ncol(Y)
  
  if(n_objs < 8){
    r = 0.225
  } else if(n_objs >= 8 && n_objs < 15){
    r = 0.26
  } else{
    r = 0.27
  }
  
  lambda = rowSums(Y)/n_objs
  
  constraints = rowSums((Y-lambda)^2 - r^2)
  
  constraints = ifelse(constraints < 0, -constraints, 0)
  
  return(constraints)
}

my_constraints_C2DTLZ2 <- function(X){
  nv <- n_variables
  
  n_constraints = 1
  
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = n_constraints + 2*nv) 
  
  colnames(Cmatrix) <- c(paste0("x",
                                rep(1:nv, times = 2),
                                rep(c("min","max"), each = nv)),
                         rep(c("g1"), each = n_constraints))
  
  # Box limits of the feasible space
  Xmin <- matrix(minimum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(maximum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  # g1 functions
  
  g1 <- constraint_C2DTLZ2
  
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)        
  
  v = rowSums(Vmatrix)  
  
  # Scaling the Penalties
  if(is.null(parent.frame(2)$iter)){
    #First generation (No incubent solutions)
    v[which(v != 0)] = (v[which(v != 0)] - min(v))/(max(v) - min(v) + 1e-16) + 0.000001
  }
  else{
    
    # Getting the incubent solutions
    e = parent.frame(2)
    Vtmatrix = e$Vt$Vmatrix
    vt = rowSums(Vtmatrix)
    
    # Extract max and min for scaling
    max = max(v,vt)
    min = min(v,vt)
    
    # Updating the new scaled penalties of the incubent solutions
    e$Vt$v[which(vt != 0)] = (vt[which(vt != 0)] - min)/(max - min + 1e-16) + 0.000001
    
    # Scaling the new solution's penalties
    v[which(v != 0)] = (v[which(v != 0)] - min)/(max - min + 1e-16) + 0.000001
  }
  
  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = v))
}
