library(MOEADr)
library(emoa)
library(ggplot2)
library(Rmisc)

source("updt_standard_save2.R")
source("MAZDA_hypervolume_file.R")

for(i in 1:20){
  file.create(sprintf("MyArchive%d.txt",i))  
}

n_objectives = 2
n_individuals = 300
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

#Definition of the problem
problem.car <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(EvaluateCommonParts(X), EvaluateWeight(X)) }
  ))
}

problem.1 <- list(name       = "problem.car",  # Function that executes the MOP
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = 2)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "SLD",H = n_individuals - 1) #Population of 50 solutions

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = floor(n_individuals*0.2), #Size of the neighborhood
                  delta.p = 1) #Probability of using the neighborhood

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- list(name = "standard_save2")

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
  nv <- 222 # number of variables
  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = 2 * nv + 54) # 296 box constraints and 36 inequality constraints
  
  # Set informative column names (be nice to your users!)
  colnames(Cmatrix) <- c(paste0("x",
                                rep(1:nv, times = 2),
                                rep(c("min","max"), each = nv)),
                         rep(c("g1"), each = 54))
  
  # Box limits of the feasible space
  Xmin <- matrix(minimum, ncol = 222, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(maximum, ncol = 222, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  # g1 and h1 functions
  g1 <- function(X){
    
    X = apply(X, FUN = Discretize,1)
    write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 222, sep = "\t")
    system(paste(paste(getwd(), "mazda_mop", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
    constraints <- scan(paste(getwd(), "Evaluate/pop_cons_eval.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = 54, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + 54)] <- -g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + 54)] <- pmax(Vmatrix[, 1:(2 * nv + 54)], 0)        # inequality constraints
  
  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}

constraint_dynamic <- function(C, alpha, bigZ, bigV, iter, ...)
{
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    identical(dim(bigZ), dim(bigV)))
  # ==========
  
  K <- (C*parent.frame(2)$iter)^alpha
  # Calculate penalized values
  bigZV <- bigZ + K * bigV
  
  # Get the selection matrix for all neighborhoods
  sel.indx <- t(apply(bigZV,
                      MARGIN = 2,
                      FUN    = order))
  
  return(sel.indx)
}
constraint<- list(name = "dynamic",
                  C = 0.05, alpha = 1.5)

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
  
  
  results2 = results
  results2$Y[,1] = results2$Y[,1]/74
  results2$Y[,2] = results2$Y[,2] - 2
  
  #Calculate the hypervolume only with feasible points
  
  #No feasible solutions
  if(max(results$V$v == 0) == 0){
    hyper[i] = 0
  }
  #At least one feasible solution
  else{
    hyper[i] = dominated_hypervolume(t(results2$Y[which(results$V$v == 0),]), (c(0,1.1)))
    cat("Iteration: ", i,"Hyper: ", hyper[i])
  }
  if(dominated_hypervolume(t(results2$Y), (c(0,1.1))) > besthyper){
    bestresults = results
    besthyper = dominated_hypervolume(t(results2$Y), (c(0,1.1)))
  }
}

NewHyper = matrix(0, nrow = 20, ncol = n_iterations)
Mean = 0
Sd = 0
for(i in 1:20){
  NewHyper[i,] = MAZDA_hypervolume_file(filename = sprintf("DATA/P3/MyArchive%d.txt",i), n_individuals = n_individuals, n_iterations = n_iterations, n_objectives = n_objectives)
}

for(i in 1:n_iterations){
  Mean[i] = mean(NewHyper[,i])
  Sd[i] = sd(NewHyper[,i])
}

points = c(1:20)
points = points*5

index = matrix(1:n_iterations,ncol = n_iterations)

b = qplot(index[points],Mean[points],fill='#A4A4A8', color="darkred")+geom_errorbar(aes(x=index[points], ymin=Mean[points]-Sd[points], ymax=Mean[points]+Sd[points]), width=0.25)

