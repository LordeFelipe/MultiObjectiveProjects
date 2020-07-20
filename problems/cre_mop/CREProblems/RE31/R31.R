library(MOEADr)
library(emoa)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("../MyFunctions/updt_standard_save.R")

file.create("MyArchive.txt")

n_individuals = 12
n_iterations = 50

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
  obj2 = matrix((20.0 * sqrt(16.0 + (X[3] * X[3]))) / (X[1] * X[3]), ncol = 1)
  obj2
}

#Objective3
Objective3 <- function(X){
  
  write(t(X),file = paste(getwd(), "/pop_vars_eval.txt", sep="/"), ncolumns = 3, sep = " ")
  system("./example", ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "pop_vars_obj.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 3, byrow = TRUE)
  
  obj2 = matrix(objectives[,3], ncol = 1)
  obj2
}

#Definition of the problem
problem.r31 <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(Objective1(X), Objective2(X), Objective3(X)) }
  ))
}

problem.1 <- list(name       = "problem.r31",  # Function that executes the MOP
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = 3)              # Number of objectives

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
  nv <- 3 # number of variables
  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = 2 * nv) 
  
  colnames(Cmatrix) <- c(paste0("x",
                                rep(1:nv, times = 2),
                                rep(c("min","max"), each = nv)))
  
  # Box limits of the feasible space
  Xmin <- matrix(minimum, ncol = 3, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(maximum, ncol = 3, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv)] <- pmax(Vmatrix[, 1:(2 * nv)], 0)        # inequality constraints

  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}
constraint<- list(name = "penalty", beta = 2)


## 10 - Execution

hyper = rep(0,20)
hyperteste = rep(0,20)
besthyper = -1
for (i in 1:1){
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
