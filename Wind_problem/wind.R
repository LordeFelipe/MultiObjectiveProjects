library(MOEADr)
library(emoa)

n_individuals = 4
n_iterations = 5

#Reading the minimum and maximum values
minmax = read.csv(file = "minmax.txt", col.names = c("a","b"), header = FALSE)

Normalize <- function(X, minmax){
  (X - minmax[,1]) / (minmax[,2] - minmax[,1])
}

#Objecive Function for annual power generation
AnnualPowerGen <- function(X){
  X <- Normalize(X,minmax)
  write(X,file = paste(getwd(), "sample/MOP/pop_vars_eval.txt", sep="/"), ncolumns = 32, sep = "\t")
  system("python windturbine_mop.py sample/MOP")
  objectives <- scan(paste(getwd(), "sample/MOP/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)
  
  gen = matrix(objectives[,1], ncol = 1)
  gen
}

#Objecive Function for Average Annual Cost
AverageAnnualCost <- function(X){
  objectives <- scan(paste(getwd(), "sample/MOP/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)
  
  cost = matrix(objectives[,2], ncol = 1)
  cost
}

#Objecive Function for Tower Base Cost
TowerbaseCost <- function(X){
  objectives <- scan(paste(getwd(), "sample/MOP/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)
  
  base = matrix(objectives[,3], ncol = 1)
  base
}

#Objecive Function for Blade Tip Speed
BladeTipSpeed <- function(X){
  objectives <- scan(paste(getwd(), "sample/MOP/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)
  
  speed = matrix(objectives[,4], ncol = 1)
  speed
}

#Objecive Function for Fatigue Damage
FatigueDamage <- function(X){
  objectives <- scan(paste(getwd(), "sample/MOP/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)
  
  damage = matrix(objectives[,5], ncol = 1)
  damage
}

#Definition of the problem
problem.wind <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(AnnualPowerGen(X), AverageAnnualCost(X), TowerbaseCost(X), BladeTipSpeed(X), FatigueDamage(X) ) }
  ))
}

problem.1 <- list(name       = "problem.wind",  # Function that executes the MOP
                  xmin       = minmax[,1],    # minimum parameter value for each dimension
                  xmax       = minmax[,2],     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = 5)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "SLD",H = n_individuals - 1)

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = floor(choose(n_individuals+4,4)*0.2), #Size of the neighborhood
                  delta.p = 1) #Probability of using the neighborhood

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- list(name = "standard")

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
  X = round(X, digits = 1)
  nv <- 32 # number of variables
  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = 2 * nv + 22) 
  
  colnames(Cmatrix) <- c(paste0("x",
                                rep(1:nv, times = 2),
                                rep(c("min","max"), each = nv)),
                         rep(c("g1"), each = 22))
  
  # Box limits of the feasible space
  Xmin <- matrix(minmax[,1], ncol = 32, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(minmax[,2], ncol = 32, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  g1 <- function(X){
    constraints <- scan(paste(getwd(), "sample/MOP/pop_cons_eval.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = 22, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + 22)] <- -g1(X) # Constraints need to be greater than zero
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + 22)] <- pmax(Vmatrix[, 1:(2 * nv + 22)], 0)        # inequality constraints
  
  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}
constraint<- list(name = "penalty", beta = 2)

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
