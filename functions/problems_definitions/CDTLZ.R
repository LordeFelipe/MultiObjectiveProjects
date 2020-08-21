constraint_C1DTLZ1 <- function(X){
  
  Y = DTLZ(X)
  sum = matrix(0,ncol = 1, nrow = nrow(X))
  
  sum = rowSums(matrix(Y[,1:ncol(Y)-1])/0.5)
  
  constraints = 1 - Y[,ncol(Y)]/0.6 - sum
  constraints = ifelse(constraints < 0, -constraints, 0)
  
  return(constraints)
}

constraint_C1DTLZ3 <- function(X){
  
  Y = DTLZ(X)
  n_objs = ncol(Y)
  
  if(n_objs <= 2){
    r = 6
  } else if(n_objs >= 3 && n_objs < 5){
    r = 9
  } else if(n_objs >= 5 && n_objs < 10){
    r = 12.5
  } else{
    r = 15
  }
  
  Y1 = rowSums(Y^2) - 16
  Y2 = rowSums(Y^2) - r^2
  
  constraints = Y1*Y2
  constraints = ifelse(constraints < 0, -constraints, 0)
  
  return(constraints)
}

constraint_C2DTLZ2 <- function(X){
  
  Y = DTLZ(X)
  n_objs = ncol(Y)
  
  if(n_objs == 3){
    r = 0.4
  } else{
    r = 0.5
  }
  
  aux1 = matrix(-Inf, nrow = nrow(X), ncol = 1)
  for (i in 1:ncol(Y)){
    aux = (Y[,i] - 1)^2
    sum = 0
    
    for (j in 1:ncol(Y)){
      if(j != i){
        sum = sum + Y[,j]^2 - r^2
      }
    }
    
    aux1 = pmax(aux1, aux + sum)
  }
  
  aux2 = matrix(rowSums((Y - 1/sqrt(n_objs))^2 - r^2), ncol = 1)
  
  constraints = pmax(aux1, aux2)
  constraints = ifelse(constraints < 0, -constraints, 0)
  
  return(constraints)
}

constraint_C3DTLZ1 <- function(X){
  
  Y = DTLZ(X)
  n_objs = ncol(Y)
  
  constraints = matrix(0, ncol = n_objs, nrow = nrow(X))
  
  for(j in 1:n_objs){
    sum = matrix(0, nrow = nrow(X))
    for(i in 1:n_objs){
      if(i != j){
        sum = sum + Y[,j]
      }
    }
    constraints[,j] = sum + Y[,i]/0.5 - 1 
  }
  
  constraints = ifelse(constraints < 0, -constraints, 0)
  return(constraints)
}

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
    constraints[,j] = sum + Y[,j]^2/4 - 1
  }
  
  constraints = ifelse(constraints < 0, -constraints, 0)
  return(constraints)
}

my_constraints <- function(X){
  nv <- n_variables
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
  
  if(problem_name == "C1-DTLZ1"){
    g1 <- constraint_C1DTLZ1
  }else if(problem_name == "C1-DTLZ3"){
    g1 <- constraint_C1DTLZ3
  }else if(problem_name == "C2-DTLZ2"){
    g1 <- constraint_C2DTLZ2
  }else if(problem_name == "C3-DTLZ1"){
    g1 <- constraint_C3DTLZ1
  }else if(problem_name == "C3-DTLZ4"){
    g1 <- constraint_C3DTLZ4
  }else{
    g1 <- constraint_C1DTLZ1
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)        
  
  v = rowSums(Vmatrix)  
  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = v))
}
