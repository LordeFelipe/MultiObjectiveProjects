# Script that plots the number of feasible solutions by generation 
# of different tests in a specific problem. The name of the test 
# should be specified in the "tests" variable. 

library(MOEADr)
library(emoa)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Path to the desired problem
# ../mazda/populations/200_generations/         -> MAZDA Car Problem
# ../moon/populations/200_generations/          -> Moon Landing Problem
# ../CRE/CRE21/     -> Problem suite Problem (To access others change the number)
path = "../moon/populations/200_generations/"

# Name of the tests
#tests = c("static1","static2","static100","selfadapting","dynamic_alpha2_C005","dynamic_alpha2_C002")
all_files = list.files(path)

tests = c(all_files[1:7])
n_cases = length(tests)

filenames = paste0(path,tests)

# Parameters for execution
n_objectives = 3
n_individuals = 325
n_iterations = 200
n_runs = 7

# Chosen generations to appear in the x axis plot
SelectedPoints = c(1:n_iterations)

# Initialization of some variables
NumberOfFeasibles = matrix(0, nrow = n_runs, ncol = n_iterations)
MeanFeasible = matrix(0, nrow = n_iterations, ncol = n_cases)
SdFeasible = matrix(0, nrow = n_iterations, ncol = n_cases)

for(nfile in 1:n_cases){
  for(i in 1:n_runs){
    # Extracting the objective values from the files
    YAll = scan(paste(getwd(), filename = sprintf(paste(filenames[nfile],"/MyArchive%d.txt",sep = ""),i), sep = "/"), quiet = TRUE)
    
    # Correcting the dimensions of the matrix
    A = matrix(YAll,nrow = n_individuals*n_iterations, ncol = n_objectives+1, byrow = TRUE)
    YAll_corrected = array(0, c(n_individuals,n_objectives+1,n_iterations))
    for(j in 1:n_iterations){
      YAll_corrected[,,j] = A[((j-1)*n_individuals+1):(n_individuals*j),]  
    }
    
    # Calculating the number of feasibles in each generation
    NumberOfFeasibles[i,] = colSums(YAll_corrected[,n_objectives+1,])
  }
  # Calculating the mean and std of each generation
  for(i in 1:n_iterations){
    MeanFeasible[i,nfile] = mean(NumberOfFeasibles[,i])
    SdFeasible[i,nfile] = sd(NumberOfFeasibles[,i])
  }
}

# Creating the labels
labels = gsub("^.*?200g_","",tests[1:n_cases])
labels = gsub("_"," ",labels)

# Creating the data frame
MeanVector = c(MeanFeasible[SelectedPoints,1:n_cases])
SdVector = c(SdFeasible[SelectedPoints,1:n_cases])
points = rep(SelectedPoints, times = n_cases)
labels = rep(labels, each = length(SelectedPoints))
dados = data.frame(FeasibleMean = MeanVector, FeasibleSd = SdVector, Generations = points, Labels = labels)

ggplot(dados, aes(x=Generations, y = FeasibleMean)) + 
  labs(x = "Generation", y = "Number of Feasible Solutions", title = "Number of feasible solutions by generation") +
  ylim(0, 60) +
  geom_point(aes(colour = Labels)) + geom_line(aes(colour = Labels))  
geom_ribbon(aes(ymin = pmax(0,FeasibleMean - FeasibleSd),ymax = pmin(300,FeasibleMean + FeasibleSd), colour = Labels),alpha=0.1)

ggsave(paste0("200g_",tests,".jpg"), device = "jpg", width = 9, height = 6)
