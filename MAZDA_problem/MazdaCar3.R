library(MOEADr)
library(emoa)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

debugSource("MyFunctions/updt_standard_save.R")
debugSource("MyFunctions/MAZDA_hypervolume_file.R")
debugSource("MyFunctions/constraint_dynamic.R")
debugSource("MyFunctions/constraint_selfadapting.R")
debugSource("MyFunctions/constraint_multistaged.R")
debugSource("MyFunctions/MAZDA.R")

# Creating the output directory if necessary
if (!file.exists("Output")){
  dir.create("Output")
} 

# Characteristics of the problem
n_variables = 222
n_objectives = 2
n_constraints = 54

# Parameters for execution
n_individuals = 30
n_iterations = 100
n_runs = 1

# Reading the possible discrete values
discrete = read.table(paste(getwd(), "DiscreteValues3.txt", sep="/"),col.names = paste0("V",seq_len(18)), sep = ",",fill = TRUE)

# Generating the minimum and maximum of each variable
maximum = c(0, nrow = n_variables)
minimum = c(0, nrow = n_variables)
for (i in 1:n_variables){
  maximum[i] = max(discrete[i,], na.rm = TRUE)
  minimum[i] = min(discrete[i,], na.rm = TRUE)
}

problem.1 <- list(name       = "problem.car",  # Function that executes the MOP
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
update <- list(name = "standard_save")

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
constraint<- list(name = "dynamic", 
                  C = 0.05, 
                  alpha = 2)

## 10 - Execution
hyper = rep(0,n_runs)
hyperteste = rep(0,n_runs)
besthyper = -1
for (i in 1:n_runs){
  file.create(sprintf("Output/MyArchive%d.txt",i)) 
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
  
  # Scaling the objectives
  results2 = results
  results2$Y[,1] = results2$Y[,1]/74
  results2$Y[,2] = results2$Y[,2] - 2
  
  # Calculate the hypervolume only with feasible points
  # No feasible solutions
  if(max(results$V$v == 0) == 0){
    hyper[i] = 0
  }
  # At least one feasible solution
  else{
    hyper[i] = dominated_hypervolume(t(results2$Y[which(results$V$v == 0),]), (c(0,1.1)))
    cat("Iteration: ", i,"Hyper: ", hyper[i])
  }
}