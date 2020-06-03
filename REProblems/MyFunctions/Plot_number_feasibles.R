# Script that plots the number of feasible solutions by generation 
# of different tests in a specific problem from the problem suite. The name of the test 
# should be specified in the "tests" variable. 

library(MOEADr)
library(emoa)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Names of the tests and their path
tests = c("static1","static2","static100","selfadapting","none","dynamic_alpha2_C005","dynamic_alpha2_C002")
path = "../DATA/CRE21/"

# Parameters for execution
n_objectives = 2
n_individuals = 300
n_iterations = 100
n_cases = length(tests)

# Initialization of some variables
NumberOfFeasibles = matrix(0, nrow = 20, ncol = n_iterations)
MeanFeasible = matrix(0, nrow = n_iterations, ncol = n_cases)
SdFeasible = matrix(0, nrow = n_iterations, ncol = n_cases)

for(nfile in 1:n_cases){
  for(i in 1:20){
    # Extracting the objective values from the files
    YAll = scan(paste(getwd(), filename = sprintf(paste(path ,tests[nfile],"/MyArchive%d.txt",sep = ""),i), sep = "/"), quiet = TRUE)
    
    # Correcting the dimensions of the matrix
    A = matrix(YAll,nrow = n_individuals*n_iterations, ncol = n_objectives+1, byrow = TRUE)
    YAll_corrected = array(0, c(n_individuals,n_objectives+1,n_iterations))
    for(j in 1:n_iterations){
      YAll_corrected[,,j] = A[((j-1)*n_individuals+1):(n_individuals*j),]  
    }
  
    # Calculating the number of feasibles in each generation
    NumberOfFeasibles[i,] = colSums(B[,n_objectives+1,])
  }
  # Calculating the mean and std of each generation
  for(i in 1:n_iterations){
    MeanFeasible[i,nfile] = mean(NumberOfFeasibles[,i])
    SdFeasible[i,nfile] = sd(NumberOfFeasibles[,i])
  }
}

# Number of the generations that are ploted in the x axis
SelectedPoints = c(1,5*c(1:20))

# Creating the data frame
MeanVector = c(MeanFeasible[SelectedPoints,1:n_cases])
SdVector = c(SdFeasible[SelectedPoints,1:n_cases])
points = rep(c(1,5*c(1:20)), times = n_cases)
labels = rep(tests[1:n_cases], each = 21)
dados = data.frame(FeasibleMean = MeanVector, FeasibleSd = SdVector, Generations = points, Labels = labels)

ggplot(dados, aes(x=Generations, y = FeasibleMean)) + 
  labs(x = "Generation", y = "Number of Feasible Solutions", title = "Dynamic Penalty Number of Feasible Solutions Evolution") +
  geom_point(aes(colour = Labels)) + geom_line(aes(colour = Labels)) + 
  geom_ribbon(aes(ymin = FeasibleMean - FeasibleSd,ymax =FeasibleMean + FeasibleSd, colour = Labels),alpha=0.1)
