library(MOEADr)
library(emoa)
library(ggplot2)
library(smoof)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

debugSource("../../functions/problems_definitions/CDTLZ.R")

problem_name = "C2-DTLZ2"

# Characteristics of the problem
n_objectives = 2
n_variables = 5

# Parameters for execution
n_individuals = 300
n_iterations = 300

minimum = rep(0, n_variables)
maximum = rep(1, n_variables)


if(problem_name == "C1-DTLZ1" || problem_name == "C1-DTLZ3" || problem_name == "C2-DTLZ2"){
  n_constraints = 1
}else{
  n_constraints = n_objectives
}

DTLZ <- make_vectorized_smoof(prob.name  = substr(problem_name,4,8),
                               dimensions = n_variables,
                               n.objectives = n_objectives)

problem.dtlz  <- list(name       = "DTLZ",
                       xmin       = rep(0, n_variables),
                       xmax       = rep(1, n_variables),
                       constraints = list(name = "my_constraints"),
                       m          = n_objectives)

## 1 - Decomposition
decomp <- list(name = "SLD",H = n_individuals-1) 

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
                       etax  = 20, pc = 1),
                  list(name  = "polymut",
                       etam  = 20, pm = 1/n_variables),
                  list(name  = "truncate"))

## 8 - Show
showpars  <- list(show.iters = "dots",
                  showevery  = 5)

## 9 - Constraint
constraint<- list(name = "penalty", beta=1)

results.dtlz <- moead(problem  = problem.dtlz,
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

plot(results.dtlz, suppress.pause = TRUE)
