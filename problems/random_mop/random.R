library(MOEADr)
library(emoa)
library("scales")
library(ggplot2)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("../../functions/updt_standard_save.R")
source("../../functions/constraint_dynamic.R")
source("../../functions/constraint_multistaged.R")
source("../../functions/constraint_selfadapting.R")
source("../../functions/constraint_unfeasible_exploration.R")

# Creating the output directory if necessary
if (!file.exists("output")){
  dir.create("output")
} 

# Characteristics of the problem
n_variables = 50
n_objectives = 2
n_constraints = 12

# Parameters for execution
n_individuals = 10
n_iterations = 10
n_runs = 1

# Generating the minimum and maximum of each variable
maximum = rep(6, n_variables)
minimum = rep(1, n_variables)

# Function responsible to convert the continuous values to the nearest discrete values
Discretize <- function(X){
  discrete = matrix(c(1,2,3,4,5,6), byrow = TRUE, ncol = 6, nrow = length(X))
  nearest = apply(abs(as.matrix(discrete) - X), 1, FUN=which.min)
  k = 0
  discreteValues = c(0)
  for (i in nearest) {
    k = k+1
    discreteValues[k] = discrete[k,i]
  }
  return(discreteValues)
}

# Objective Function
EvaluateRandom1 <- function(X){
  
  X = Discretize(X)
  Xstring = paste("\"",paste(X, collapse=""),"\"", sep ="")
  
  write(Xstring,file = paste(getwd(), "random.json", sep="/"))
  objectives = system(paste("python3 eccomp2020/rngbias.py --objectives \"[[1, 2, 3, 4, 5, 6, 7], [8, 9, 10, 11, 12, 13, 14, 15]]\" < random.json"), intern = TRUE)
  objectives = strsplit(objectives, " ")
  objective1 = as.double(gsub("[[,]","",objectives[[1]][2]))
  objective2 = as.double(gsub("[],]","",objectives[[1]][3]))
  write.csv(c(objective1,objective2), file = paste(getwd(), "objs.json", sep="/"))
  
  obj1 = matrix(objective1, ncol = 1)
  obj1
}

EvaluateRandom2 <- function(X){
  
  objectives <- read.csv("objs.json")
  
  obj2 = matrix(objectives[[2]][2], ncol = 1)
  obj2
}

# Definition of the problem
problem.random <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(EvaluateRandom1(X), EvaluateRandom2(X)) }
  ))
}

problem.1 <- list(name       = "problem.random",
                  xmin       = minimum,
                  xmax       = maximum, 
                  m          = n_objectives)

## 1 - Decomposition
decomp <- list(name = "SLD",H = n_individuals - 1) 

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = floor(n_variables*0.2), #Size of the neighborhood
                  delta.p = 0.9) #Probability of using the neighborhood

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
variation <- list(list(name     = "diffmut",
                       basis    = "rand",
                       phi      = 0.5),
                  list(name  = "polymut",
                       etam  = 20, pm = 1/n_variables ),
                  list(name  = "truncate"))

## 8 - Show
showpars  <- list(show.iters = "dots",
                  showevery  = 5)

## 9 - Constraint
constraint<- list(name = "unfeasible_exploration",
                  penalties = c(0,1,1000))

## 10 - Execution
for (i in 1:n_runs){
  file.create(sprintf("output/MyArchive%d.txt",i)) 
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



#a = c(1,2)
#text = paste0("[",a[1],",",a[2],"]")
#system(paste0("echo '",text ,"' | opt submit --match=1"))
