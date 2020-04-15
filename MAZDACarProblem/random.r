library(MOEADr)
library(emoa)

#(X)ARRUMAR ESSE CODIGO CREDO
#(X)GERAR VALORES ALEATORIOS COM BASE NA FAIXA CORRETA DE CADA VARIAVEL
#(X)GERAR BOXPLOT DE 20 TENTATIVAS COM 300k SOLUCOES
#(X)GERAR GRAFICO DE DISTRIBUIÇOES 

#Table with the possible values of each variable
discrete = read.table(paste(getwd(), "DiscreteValues3.txt", sep="/"),col.names = paste0("V",seq_len(18)), sep = ",",fill = TRUE)

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

#Receive the solutions and outputs the objective values
Evaluate <- function(X){
  write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = 222, sep = "\t")
  system(paste(paste(getwd(), "mazda_mop", sep = "/"), paste(getwd(), "Evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)
  
  weight = matrix(objectives[,1:2], ncol = 2)
  weight
}

#Receive the solutions and outputs the constraints value
g1 <- function(X){
  constraints <- scan(paste(getwd(), "Evaluate/pop_cons_eval.txt", sep = "/"), quiet = TRUE)
  constraints <- matrix(constraints, ncol = 54, byrow = TRUE)
  return(constraints)
}

#Initialing variables
random = matrix(0, nrow = 30000, ncol = 222)
maximum = c(0, nrow = 222)
minimum = c(0, nrow = 222)

#Generating the minimum and maximum of each variable
for (i in 1:222){
  maximum[i] = max(discrete[i,], na.rm = TRUE)
  minimum[i] = min(discrete[i,], na.rm = TRUE)
}

#Generating the random solutions in the especific range of each variable
for(i in 1:222){
  random[,i] = matrix(runif(30000, min = minimum[i], max = maximum[i]), nrow = 30000, ncol = 1)
}

#Discretizing and evaluating the solutions
discreteValues = apply(random, FUN = Discretize,1)
objectiveValues = Evaluate(discreteValues)

#Generating the constraints
constraints = g1(discreteValues)

plot(objectiveValues)
normalizedObjectives = objectiveValues

#Normalizing the solutions
normalizedObjectives[,1] = objectiveValues[,1] - 2
normalizedObjectives[,2] = objectiveValues[,2]/74

#Verifying the minimum constraint of each solution 
minConstraint = apply(constraints, 1, min)

#Printing the constraints that were higher than the threshold
minConstraint[which(minConstraint > -0.2)]

#Calculating the hypervolume
dominated_hypervolume(t(normalizedObjectives),(c(0,2)))

#Ploting the frequency of which constraint is the hardest to minimize
tableConstraints = table(apply(constraints, 1, FUN=which.min))
plot(tableConstraints)


pl = rowSums(t(constraints < 0))
barplot(pl, names.arg = seq(54))

