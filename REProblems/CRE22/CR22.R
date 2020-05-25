library(MOEADr)
library(emoa)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

debugSource("../MyFunctions/constraint_dynamic.R")
debugSource("../MyFunctions/constraint_selfadapting.R")
debugSource("../MyFunctions/constraint_multistaged.R")
debugSource("../MyFunctions/updt_standard_save.R")

for(i in 1:20){
  file.create(sprintf("MyArchive%d.txt",i))  
}

#Characteristics of the problem
n_variables = 4
n_objectives = 2
n_constraints = 4

#Parameters for execution
n_individuals = 300
n_iterations = 100

#Creating Variable Bounds
minimum = c(0.125, 0.1, 0.1, 0.125)
maximum = c(5.0, 10.0, 10.0, 5.0)

#Objective1
Objective1 <- function(X){ 
  
  obj1 = matrix((1.10471 * X[1] * X[1] * X[2]) + (0.04811 * X[3] * X[4]) * (14.0 + X[2]), ncol = 1)
  obj1
}

#Objective2
Objective2 <- function(X){
  
  P = 6000
  L = 14
  E = 30 * 1e6
  f = (4 * P * L * L * L) / (E * X[4] * X[3] * X[3] * X[3]);
  obj2 = matrix(f, ncol = 1)
  obj2
}

#Definition of the problem
problem.cr22 <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(Objective1(X), Objective2(X)) }
  ))
}

problem.1 <- list(name       = "problem.cr22",  # Function that executes the MOP ###
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = n_objectives)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "SLD",H = n_individuals - 1)

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = 20, #Size of the neighborhood
                  delta.p = 0.9) #Probability of using the neighborhood

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- list(name = "standard_save", UseArchive = FALSE)

## 5 - Scaling
scaling <- list(name = "simple")

## 6 - Stop criterion
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = n_iterations))

## 7 - Variation Operators
variation <- list(list(name  = "sbx",
                       etax  = 20, pc = 0.7),
                  list(name  = "polymut",
                       etam  = 20, pm = 0.3 ),
                  list(name  = "truncate"))

## 8 - Show
showpars  <- list(show.iters = "dots",
                  showevery  = 20)

## 9 - Constraint
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
                         rep(c("g1"), each = n_constraints))
  
  # Box limits of the feasible space
  Xmin <- matrix(minimum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(maximum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  g1 <- function(X){
    write(t(X),file = paste(getwd(), "/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = " ")
    system("./example", ignore.stdout = TRUE)
    constraints <- scan(paste(getwd(), "pop_vars_cons.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = n_constraints, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- -g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)  # inequality constraints
  
  v = rowSums(Vmatrix)  
  if(is.null(parent.frame(2)$iter)){
    v[which(v != 0)] = (v[which(v != 0)] - min(v))/(max(v) - min(v)) + 0.000001
  }
  else{
    e = parent.frame(2)
    Vtmatrix = e$Vt$Vmatrix
    vt = rowSums(Vtmatrix)
    e$Vt$v[which(vt != 0)] = (vt[which(vt != 0)] - min(v,vt))/(max(v,vt) - min(v,vt)) + 0.000001
    v[which(v != 0)] = (v[which(v != 0)] - min(v,vt))/(max(v,vt) - min(v,vt)) + 0.000001
  }
  
  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = v))
}
constraint<- list(name = "multistaged", beta = 0.5)

## 10 - Execution

for (i in 1:20){
  cat("\nIteration: ", i)
  results <- moead(problem  = problem.1,
                   decomp = decomp,
                   neighbors = neighbors,
                   aggfun = aggfun,
                   scaling = scaling,
                   constraint = constraint,
                   stopcrit = stopcrit,
                   update = update,
                   variation = variation,
                   showpars = showpars,
                   seed     = floor(runif(1)*1000))
}

