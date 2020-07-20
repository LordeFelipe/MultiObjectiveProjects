# Script used to run the problems from RE problem suite
# Don't forget to change the desired problem in the following points:
# 1. In debugSource
# 2. In the parameters of CRE_parameters
# 3. In the name of the problem at the list of "problem.1"

library(MOEADr)
library(emoa)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

debugSource("../../functions/problem_definitions/CRE21.R")
debugSource("../../functions/CRE_parameters.R")

debugSource("../../functions/updt_standard_save.R")
debugSource("../../functions/constraint_dynamic.R")
debugSource("../../functions/constraint_multistaged.R")
debugSource("../../functions/constraint_selfadapting.R")

# Creating the output directory if necessary
if (!file.exists("Output")){
  dir.create("Output")
} 

# Getting important parameters from the CRE problem
info = CRE_parameters("CRE21")

# Characteristics of the problem
n_variables = info$n_variables
n_objectives = info$n_objectives
n_constraints = info$n_constraints

# 300 -> 300 (2 obj), 25 -> 325 (3 obj), 7 -> 210 (5 obj)
# Parameters for execution
n_individuals = 300
n_iterations = 100
n_runs = 20

# Creating Variable Bounds
minimum = info$minimum
maximum = info$maximum

## 0 - Definition of the problem
problem.1 <- list(name       = "problem.cr21",  # Function that executes the MOP
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"), # Constraint functions
                  m          = n_objectives)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "SLD",
               H = n_individuals - 1)

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = 20, #Size of the neighborhood
                  delta.p = 0.9) #Probability of using the neighborhood

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- list(name = "standard_save", 
               UseArchive = FALSE)

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
constraint<- list(name = "vbr", type = "sr", pf = 0.1)

## 10 - Execution
for (i in 1:n_runs){
  file.create(sprintf("Output/MyArchive%d.txt",i))  
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