#Objective1
Objective1 <- function(X){ 
  obj1 = matrix(1.98 + 4.9 * X[1] + 6.67 * X[2] + 6.98 * X[3] + 4.01 * X[4] + 1.78 * X[5] + 0.00001 * X[6] + 2.73 * X[7], ncol = 1)
  obj1
}

#Objective2
Objective2 <- function(X){
  obj2 = matrix(4.72 - 0.5 * X[4] - 0.19 * X[2] * X[3], ncol = 1)
  obj2
}

#Objective3
Objective3 <- function(X){
  Vmbp = 10.58 - 0.674 * X[1] * X[2] - 0.67275 * X[2];
  Vfd = 16.45 - 0.489 * X[3] * X[7] - 0.843 * X[5] * X[6];
  obj3 = matrix(0.5 * (Vmbp + Vfd), ncol = 1)
  obj3
}

#Definition of the problem
problem.cr31 <- function(X) { ###
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(Objective1(X), Objective2(X), Objective3(X)) } ###
  ))
}

my_constraints <- function(X)
{
  nv <- n_variables # number of variables
  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = 2 * nv + n_constraints) 
  
  colnames(Cmatrix) <- c(paste0("x",
                                rep(1:nv, times = 2),
                                rep(c("min","max"), each = nv)),
                         paste0("g",
                                rep(1:n_constraints, times = 1)))
  
  # Box limits of the feasible space
  Xmin <- matrix(minimum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(maximum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  g1 <- function(X){
    write(t(X),file = paste(getwd(), "CREProblems/CRE31/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = " ")
    system("CREProblems/CRE31/example", ignore.stdout = TRUE)
    constraints <- scan(paste(getwd(), "CREProblems/CRE31/pop_vars_cons.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = n_constraints, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  
  # Inequality constraints
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)  
  
  v = rowSums(Vmatrix)  
  # Before the first generation when there is no incubent solutions to scale yet
  if(is.null(parent.frame(2)$iter)){
    v[which(v != 0)] = (v[which(v != 0)] - min(v))/(max(v) - min(v)) + 0.000001
  }
  # Case of all other generations
  else{
    e = parent.frame(2)
    Vtmatrix = e$Vt$Vmatrix
    vt = rowSums(Vtmatrix)
    e$Vt$v[which(vt != 0)] = (vt[which(vt != 0)] - min(v,vt))/(max(v,vt) - min(v,vt)) + 0.000001
    v[which(v != 0)] = (v[which(v != 0)] - min(v,vt))/(max(v,vt) - min(v,vt)) + 0.000001
  }
  
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = v))
}