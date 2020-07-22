#Objective1
Objective1 <- function(X){ 
  obj1 = matrix(106780.37 * (X[2] + X[3]) + 61704.67, ncol = 1)
  obj1
}

#Objective2
Objective2 <- function(X){
  obj2 = matrix(3000 * X[1], ncol = 1)
  obj2
}

#Objective3
Objective3 <- function(X){
  obj3 = matrix(305700 * 2289 * X[2] / ((0.06*2289)^0.65), ncol = 1)
  obj3
}

#Objective4
Objective4 <- function(X){
  obj4 = matrix(250 * 2289 * exp(-39.75*X[2]+9.9*X[3]+2.74), ncol = 1)
  obj4
}

#Objective5
Objective5 <- function(X){
  obj5 = matrix(25 * (1.39 /(X[1]*X[2]) + 4940*X[3] -80), ncol = 1)
  obj5
}

#Definition of the problem
problem.cre51 <- function(X) { ###
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(Objective1(X), Objective2(X), Objective3(X), Objective4(X), Objective5(X)) } ###
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
    write(t(X),file = paste(getwd(), "CREProblems/CRE51/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = " ")
    system("CREProblems/CRE51/example", ignore.stdout = TRUE)
    constraints <- scan(paste(getwd(), "CREProblems/CRE51/pop_vars_cons.txt", sep = "/"), quiet = TRUE)
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
              v       = rowSums(Vmatrix)))
}