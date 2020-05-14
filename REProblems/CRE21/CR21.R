library(MOEADr)
library(emoa)

source("../MyFunctions/updt_standard_save2.R")
source("../MyFunctions/CRE2_hypervolume_file.R")
source("../MyFunctions/constraint_dynamic.R")
source("../MyFunctions/scalarization_wt_selfadapting.R")
source("../MyFunctions/constraint_selfadapting.R")

for(i in 1:20){
  file.create(sprintf("MyArchive%d.txt",i))  
}

#Characteristics of the problem
n_variables = 3
n_objectives = 2
n_constraints = 3

#Parameters for execution
n_individuals = 300
n_iterations = 100

#Creating Variable Bounds
minimum = c(0.0001, 0.0001, 1.0)
maximum = c(100.0, 100.0, 3.0)

#Objective1
Objective1 <- function(X){ 

  obj1 = matrix(X[1] * sqrt(16.0 + (X[3] * X[3])) + X[2] * sqrt(1.0 + X[3] * X[3]), ncol = 1)
  obj1
}

#Objective2
Objective2 <- function(X){
  
  obj2 = matrix(((20.0 * sqrt(16.0 + (X[3] * X[3]))) / (X[1] * X[3])), ncol = 1)
  obj2
}

#Definition of the problem
problem.cr21 <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(Objective1(X), Objective2(X)) }
  ))
}

problem.1 <- list(name       = "problem.cr21",  # Function that executes the MOP
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
update <- list(name = "standard_save2", UseArchive = FALSE)

## 5 - Scaling
scaling <- list(name = "simple")

## 6 - Stop criterion
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = n_iterations))

## 7 - Variation Operators
variation <- list(list(name  = "sbx",
                       etax  = 20, pc = 0.7),
                  list(name  = "polymut",
                       etam  = 20, pm = 0.7 ),
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
    
    constraints = matrix(0,nrow = n_constraints, ncol = n_individuals)
    obj1 = matrix(X[,1] * sqrt(16.0 + (X[,3] * X[,3])) + X[,2] * sqrt(1.0 + X[,3] * X[,3]), ncol = 1)
    obj2 = matrix(((20.0 * sqrt(16.0 + (X[,3] * X[,3]))) / (X[,1] * X[,3])), ncol = 1)
    constraints[1,] = 0.1 - obj1
    constraints[2,] = 100000.0 - obj2;
    constraints[3,] = 100000 - ((80.0 * sqrt(1.0 + X[,3] * X[,3])) / (X[,3] * X[,2]));
    
    for (k in 1:n_constraints) {
      for(l in 1:n_individuals){
        if(constraints[k,l] < 0){
          constraints[k,l] = -constraints[k,l]
        } 
        else{
          constraints[k,l] = 0
        }
      }
    }
    return(constraints)
  } 
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)        # inequality constraints
  
  # Normalizing the violations
  v = rowSums(Vmatrix)
  v[which(v != 0)] = (v[which(v != 0)] - min(v))/(max(v) - min(v)) + 0.00001
  
  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = v))
}
constraint<- list(name = "selfadapting", beta = 0.2)


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
  
  
  #Calculate the hypervolume only with feasible points
  #If there is no feasible solutions, the hypervolume is 1
  if(max(results$V$v == 0) == 0){
    hyper[i] = 0
  }
  
  #At least one feasible solution
  else{
    #Only use the solutions which violates none of the constraints
    hyper[i] = dominated_hypervolume(t(normalized[which(results$V$v == 0),]), (c(1.1,1.1)))
    cat("Iteration: ", i,"Hyper: ", hyper[i])
  }
  
  #Saves the best result
  if(hyper[i] > besthyper){
    bestresults = results
    besthyper = hyper[i]
  }
}

#NewHyper = CRE2_hypervolume_file(filename = "MyArchive.txt", n_individuals = n_individuals, n_iterations = n_iterations, n_objectives = n_objectives)

index = matrix(1:n_iterations,ncol = n_iterations)
plot(index, NewHyper)
