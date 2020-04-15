library(MOEADr)
library(emoa)

start_time <- Sys.time()

n_individuals = 24
n_iterations = 100

#Reading the possible discrete values
discrete = read.table(paste(getwd(), "DiscreteValues3.txt", sep="/"),col.names = paste0("V",seq_len(18)), sep = ",",fill = TRUE)

maximum = c(0, nrow = 222)
minimum = c(0, nrow = 222)

#Generating the minimum and maximum of each variable
for (i in 1:222){
  maximum[i] = max(discrete[i,], na.rm = TRUE)
  minimum[i] = min(discrete[i,], na.rm = TRUE)
}

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
  write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 222, sep = "\t")
  system(paste(paste(getwd(), "mazda_mop", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)

  weight = matrix(objectives[,1], ncol = 1)
  weight
}

#Objective Function for the number of commom parts
EvaluateCommonParts <- function(X){

  X = Discretize(X)

  write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 222, sep = "\t")
  system(paste(paste(getwd(), "mazda_mop", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)

  common = matrix(objectives[,2], ncol = 1)
  common
}

#Objecive Function for the constraint violation
EvaluateConstraints <- function(X)
{
  X = Discretize(X)
  nv <- 222 # number of variables
  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = 1,
                    ncol = 2 * nv + 54) # 296 box constraints and 36 inequality constraints
  
  colnames(Cmatrix) <- c(paste0("x",
                                rep(1:nv, times = 2),
                                rep(c("min","max"), each = nv)),
                         rep(c("g1"), each = 54))
  
  # Box limits of the feasible space
  Xmin <- minimum
  Xmax <- maximum
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  g1 <- function(X){
    
    write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 222, sep = "\t")
    system(paste(paste(getwd(), "mazda_mop", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
    constraints <- scan(paste(getwd(), "Evaluate/pop_cons_eval.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = 54, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[(2*nv + 1):(2*nv + 54)] <- -g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + 54)] <- pmax(Vmatrix[, 1:(2 * nv + 54)], 0)        # inequality constraints
  
  # Return necessary variables
  return(rowSums(Vmatrix))
}

#Definition of the problem
problem.car <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(EvaluateCommonParts(X), EvaluateWeight(X), EvaluateConstraints(X)) }
  ))
}

problem.1 <- list(name       = "problem.car",  # Function that executes the MOP
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
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

constraint<- list(name = "none")

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
  
  #Normalizing the objective values
  results2 = results
  results2$Y[,1] = results2$Y[,1]/74
  results2$Y[,2] = results2$Y[,2] - 2

  #Calculate the hypervolume only with feasible points
  end_time <- Sys.time()
  #If there is no feasible solutions, the hypervolume is 0
  if(max(results$Y[,3] == 0) == 0){
    hyper[i] = 0
    cat("Iteration: ", i,"Hyper: ", hyper[i])
    cat("Time: ", start_time - end_time)
  }
  
  #At least one feasible solution
  else{
    #Only use the solutions which violates none of the constraints
    hyper[i] = dominated_hypervolume(t(results2$Y[which(results$Y[,3] == 0),1:2]), (c(0,1.1)))
    cat("Iteration: ", i,"Hyper: ", hyper[i])
    cat("Time: ", start_time - end_time)
  }
  #Saves the best result
  if(hyper[i] > besthyper){
    bestresults = results
    besthyper = hyper[i]
  }
}

end_time <- Sys.time()
end_time - start_time