library(MOEADr)
library(emoa)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

debugSource("../../functions/problem_definitions/WIND.R")

debugSource("../../functions/updt_standard_save.R")
debugSource("../../functions/constraint_dynamic.R")
debugSource("../../functions/constraint_multistaged.R")
debugSource("../../functions/constraint_selfadapting.R")

# Creating the output directory if necessary
if (!file.exists("Output")){
  dir.create("Output")
} 

# Characteristics of the problem
n_variables = 32
n_objectives = 5
n_constraints = 22

# Parameters for execution
n_individuals = 7 # with SLD it means 210 solutions
n_iterations = 50
n_runs = 10

# Generating the minimum and maximum of each variable
minmax = read.csv(file = "minmax.txt", col.names = c("min","max"), header = FALSE)
min = minmax[,1]
max = minmax[,2]

problem.1 <- list(name       = "problem.wind",  # Function that executes the MOP
                  xmin       = min,    # minimum parameter value for each dimension
                  xmax       = max,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = n_objectives)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "SLD",H = n_individuals - 1)

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = 20, #Size of the neighborhood
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

## 9 - Constraints
constraint<- list(name = "penalty", beta = 100)

## 10 -Execution
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
  
  normalized_min = c(-2.42e+07,9.47e+05,2.09e+07,3.20e+01,-2.30e+00)
  normalized_max = c(-1.96e+02,2.35e+06,1.89e+08,8.00e+01,-2.50e-05)

}