library(MOEADr)

#Reading the possible discrete values
discrete = read.table(paste(getwd(), "DiscreteValues.txt", sep="/"),col.names = paste0("V",seq_len(18)), sep = ",",fill = TRUE)

#Function responsible to convert the continuous values to discrete values
Discretize <- function(X){
  nearest = apply(abs(as.matrix(discrete) - X), 1, FUN=which.min)
  k = 0
  discreteValues = c(0)
  for (i in nearest) {
    k = k+1
    discreteValues[k] = discrete[k,i]
  }
  return(discreteValues)
}

#Objecive Function for the total weight
EvaluateWeight <- function(X){
  
  X = Discretize(X)
  write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 148, sep = "\t")
  system(paste(paste(getwd(), "mazda_mop_sca", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "))
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"))
  objectives <- matrix(objectives, ncol = 4, byrow = TRUE)
  
  weight = matrix(objectives[,1], ncol = 1)
  weight
}

#Objective Function for the number of commom parts
EvaluateCommonParts <- function(X){
  
  X = Discretize(X)
  
  write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 148, sep = "\t")
  system(paste(paste(getwd(), "mazda_mop_sca", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "))
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"))
  objectives <- matrix(objectives, ncol = 4, byrow = TRUE)
  
  common = matrix(objectives[,2], ncol = 1)
  common
}

#Definition of the problem
problem.car <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(EvaluateCommonParts(X), EvaluateWeight(X)) } 
  ))
}

problem.1 <- list(name       = "problem.car",  # Function that executes the MOP
                  xmin       = rep(0.3,148),    # minimum parameter value for each dimension
                  xmax       = rep(3,148),     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = 2)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "SLD",H = 99) #Population of 50 solutions

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = 2, #Size of the neighborhood
                  delta.p = 0.9) #Probability of using the neighborhood

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- list(name = "standard")

## 5 - Scaling
scaling <- list(name = "simple")

## 6 - Stop criterion
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = 200))

## 7 - Variation Operators
variation <- list(list(name  = "sbx",
                       etax  = 20, pc = 0.7),
                  list(name  = "polymut",
                       etam  = 20, pm = 0.3),
                  list(name  = "truncate"))

## 8 - Show
showpars  <- list(show.iters = "dots",
                  showevery  = 10)

## 9 - Constraint
my_constraints <- function(X)
{
  X = round(X, digits = 1)
  nv <- 148 # number of variables
  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(), 
                    nrow = nrow(X),
                    ncol = 2 * nv + 36) # 296 box constraints and 36 inequality constraints
  
  # Set informative column names (be nice to your users!)
  colnames(Cmatrix) <- c(paste0("x", 
                                rep(1:nv, times = 2), 
                                rep(c("min","max"), each = nv)),
                         rep(c("g1"), each = 36))
  
  # Box limits of the feasible space
  Xmin <- matrix(0.3, byrow = TRUE, ncol = 148, nrow = nrow(X))
  Xmax <- matrix(3, byrow = TRUE, ncol = 148, nrow = nrow(X))
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  # g1 and h1 functions
  g1 <- function(X){
    
    X = apply(X, FUN = Discretize,1)
    write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 148, sep = "\t")
    system(paste(paste(getwd(), "mazda_mop_sca", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "))
    constraints <- scan(paste(getwd(), "Evaluate/pop_cons_eval.txt", sep = "/"))
    constraints <- matrix(constraints, ncol = 36, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + 36)] <- -g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + 36)] <- pmax(Vmatrix[, 1:(2 * nv + 36)], 0)        # inequality constraints
  
  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
  
}
constraint<- list(name = "penalty",
                  beta = 100)

## 10 - Execution
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
                 seed     = 15)

summary(results)
plot(results, suppress.pause = TRUE)
