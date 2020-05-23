library(MOEADr)
library(emoa)

source("../MyFunctions/updt_standard_save2.R")
debugSource("../MyFunctions/CRE2_hypervolume_file.R")
debugSource("../MyFunctions/constraint_dynamic.R")
debugSource("../MyFunctions/constraint_selfadapting.R")

#Characteristics of the problem
n_variables = 3
n_objectives = 5
n_constraints = 7

#Parameters for execution
n_individuals = 210
n_iterations = 500

#Creating Variable Bounds
minimum = c(0.01,0.01,0.01) ###
maximum = c(0.45,0.1,0.1) ###

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
problem.cr51 <- function(X) { ###
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(Objective1(X), Objective2(X), Objective3(X), Objective4(X), Objective5(X)) } ###
  ))
}

problem.1 <- list(name       = "problem.cr51",  # Function that executes the MOP ###
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = n_objectives)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "uniform",N = n_individuals, nobj = n_objectives)

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = floor(3), #Size of the neighborhood
                  delta.p = 1) #Probability of using the neighborhood

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- list(name = "standard", UseArchive = FALSE)

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
                  showevery  = 5)

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
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)        # inequality constraints

  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}
constraint<- list(name = "selfadapting")


## 10 - Execution

hyper = rep(0,20)
hyperteste = rep(0,20)
besthyper = -1
for (i in 1:20){
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
  #Normalizing Objective Funtions
  normalized = results$Y
  normalized[,1] = (results$Y[,1] - results$ideal[1])/(results$nadir[1] - results$ideal[1])
  normalized[,2] = (results$Y[,2] - results$ideal[2])/(results$nadir[2] - results$ideal[2])
  normalized[,3] = (results$Y[,3] - results$ideal[3])/(results$nadir[3] - results$ideal[3])
  normalized[,4] = (results$Y[,4] - results$ideal[4])/(results$nadir[4] - results$ideal[4])
  normalized[,5] = (results$Y[,5] - results$ideal[5])/(results$nadir[5] - results$ideal[5])
  
  
  #Calculate the hypervolume only with feasible points
  #If there is no feasible solutions, the hypervolume is 1
  if(max(results$V$v == 0) == 0){
    hyper[i] = 0
  }
  
  #At least one feasible solution
  else{
    #Only use the solutions which violates none of the constraints
    hyper[i] = dominated_hypervolume(t(normalized[which(results$V$v == 0),]), (c(1.1,1.1,1.1,1.1,1.1))) ###
    cat("Iteration: ", i,"Hyper: ", hyper[i])
  }
  
  #Saves the best result
  if(hyper[i] > besthyper){
    bestresults = results
    besthyper = hyper[i]
  }
}
