constraint_C3DTLZ4 <- function(X){
  Y = DTLZ(X)
  n_objs = ncol(Y)
  
  constraints = matrix(0, ncol = n_objs, nrow = nrow(X))
  
  for(j in 1:n_objs){
    sum = matrix(0, nrow = nrow(X))
    for(i in 1:n_objs){
      if(i != j){
        sum = sum + Y[,i]^2 
      }
    }
    constraints[,j] = sum + Y[,j]^2/4 -1
  }
  
  constraints = ifelse(constraints < 0, -constraints, 0)
  return(constraints)
}

my_constraints_C3DTLZ4 <- function(X){
  nv <- n_variables
  
  n_constraints = n_objectives
  minimum = rep(0, n_variables)
  maximum = rep(1, n_variables)
  
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
  
  g1 <- constraint_C3DTLZ4
  
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
