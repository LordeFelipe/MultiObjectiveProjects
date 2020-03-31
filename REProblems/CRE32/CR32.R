library(MOEADr)
library(emoa)

#Characteristics of the problem
n_variables = 6
n_objectives = 3
n_constraints = 9

#Parameters for execution
n_individuals = 16
n_iterations = 20

#Creating Variable Bounds
minimum = c(150,20,13,10,14,0.63) ###
maximum = c(274.32,32.31,25.0,11.71,18.0,0.75) ###

#Objective1
Objective1 <- function(X){ 
  
  write(t(X),file = paste(getwd(), "/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = " ") 
  system("./example", ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "pop_vars_obj.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = n_objectives, byrow = TRUE) 
  
  obj1 = matrix(objectives[,1], ncol = 1)
  obj1
}

#Objective2
Objective2 <- function(X){
  
  write(t(X),file = paste(getwd(), "/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = " ") 
  system("./example", ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "pop_vars_obj.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = n_objectives, byrow = TRUE) ###
  
  obj2 = matrix(objectives[,2], ncol = 1)
  obj2
}

#Objective3
Objective3 <- function(X){
  
  write(t(X),file = paste(getwd(), "/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = " ") ###
  system("./example", ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "pop_vars_obj.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = n_objectives, byrow = TRUE) ###
  
  obj3 = matrix(objectives[,3], ncol = 1)
  obj3
}

#Definition of the problem
problem.cr32 <- function(X) { ###
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(Objective1(X), Objective2(X), Objective3(X)) } ###
  ))
}

problem.1 <- list(name       = "problem.cr32",  # Function that executes the MOP ###
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = n_objectives)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "SLD",H = n_individuals - 1)

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = floor(n_individuals*0.2), #Size of the neighborhood
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
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)        # inequality constraints

  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}
constraint<- list(name = "penalty", beta = 0.5)


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
  
  #Calculate the hypervolume only with feasible points
  #If there is no feasible solutions, the hypervolume is 1
  if(max(results$V$v == 0) == 0){
    hyper[i] = 0
  }
  
  #At least one feasible solution
  else{
    #Only use the solutions which violates none of the constraints
    hyper[i] = dominated_hypervolume(t(normalized[which(results$V$v == 0),]), (c(1.1,1.1,1.1))) ###
    cat("Iteration: ", i,"Hyper: ", hyper[i])
  }
  
  #Saves the best result
  if(hyper[i] > besthyper){
    bestresults = results
    besthyper = hyper[i]
  }
}
