library(mco)

#( )LEMBRAR DE APLICAR OS CONSTRAINTS
#( )RODAR O NSGA2 20 vezes com 100/300

discrete = read.table(paste(getwd(), "DiscreteValues3.txt", sep="/"),col.names = paste0("V",seq_len(18)), sep = ",",fill = TRUE)

#Function responsible to convert the continuous values to discrete values
Discretize <- function(X){

  #Reading the possible discrete values
  discrete = read.table(paste(getwd(), "DiscreteValues3.txt", sep="/"),col.names = paste0("V",seq_len(18)), sep = ",",fill = TRUE)

  nearest = apply(abs(as.matrix(discrete) - X), 1, FUN=which.min)
  k = 0
  discreteValues = c(0)
  for (i in nearest) {
    k = k+1
    discreteValues[k] = discrete[k,i]
  }
  return(discreteValues)
}

#Objecive Functions
Evaluate <- function(X){

  X = Discretize(X)
  write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 222, sep = "\t")
  system(paste(paste(getwd(), "mazda_mop", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)

  objectives = matrix(objectives[,1:2], ncol = 1)
  return(objectives)
}

MultipleEvaluate <- function(X){
  r = apply(X, FUN = Evaluate,1)
  return(r)
}

g1 <- function(X){

  #X = apply(X, FUN = Discretize,1)
  write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 222, sep = "\t")
  system(paste(paste(getwd(), "mazda_mop", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
  constraints <- scan(paste(getwd(), "Evaluate/pop_cons_eval.txt", sep = "/"), quiet = TRUE)
  constraints <- matrix(constraints, ncol = 54, byrow = TRUE)
  return(t(constraints))
}

maximum = c(0, nrow = 222)
minimum = c(0, nrow = 222)

#Generating the minimum and maximum of each variable
for (i in 1:222){
  maximum[i] = max(discrete[i,], na.rm = TRUE)
  minimum[i] = min(discrete[i,], na.rm = TRUE)
}

results = nsga2(MultipleEvaluate, 222, 2,
                generations = 300, popsize = 100,
                cprob = 0.7,mprob = 0.3, cdist = 20, lower.bounds=minimum,
                upper.bounds=maximum, mdist = 20,
                constraints = g1,
                cdim = 54,
                vectorized = TRUE)

#Discretizing and evaluating the solutions
discreteValues = apply(results$par, FUN = Discretize,1)

#Generating the constraints
constraints = g1(discreteValues)

#Verifying the minimum constraint of each solution
minConstraint = apply(t(constraints), 1, min)

#Printing the constraints that were higher than the threshold
minConstraint[which(minConstraint > -0.2)]

#Ploting the frequency of which constraint is the hardest to minimize
tableConstraints = table(apply(t(constraints), 1, FUN=which.min))
plot(tableConstraints)
