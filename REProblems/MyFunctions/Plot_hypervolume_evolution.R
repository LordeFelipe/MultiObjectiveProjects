# Script used to plot the hypervolume evolution of different tests
# in a specific problem from the problem suite. The name of the test 
# should be specified in the "tests" variable. 

library(MOEADr)
library(emoa)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

debugSource("../MyFunctions/CRE_hypervolume_evolution.R")

# Names of the tests and their path
tests = c("none","static1","static100","selfadapting","dynamic_alpha2_C005","dynamic_alpha2_C002")
filenames = paste0("../DATA/CRE31/",tests)

# Parameters for execution
n_objectives = 3
n_individuals = 325
n_iterations = 100
n_cases = length(tests)

# Initialization of some variables
NewHyper = matrix(0, nrow = 20, ncol = n_iterations)
Mean = matrix(0, nrow = n_iterations, ncol = n_cases)
Median = matrix(0, nrow = n_iterations, ncol = n_cases)
Sd = matrix(0, nrow = n_iterations, ncol = n_cases)
dados = data.frame

# Calculating the mean and standard deviation of each iteration of all the tests
for(nfile in 1:n_cases){
  NewHyper = CRE_hypervolume_evolution(case = nfile, filename = filenames, n_individuals = n_individuals, n_iterations = n_iterations, n_objectives = n_objectives)
  for(i in 1:n_iterations){
    Median[i,nfile] = median(NewHyper[,i])
    Mean[i,nfile] = mean(NewHyper[,i])
    Sd[i,nfile] = sd(NewHyper[,i])
  }
}

# Number of the generations that are ploted in the x axis
SelectedPoints = c(1:100)

# Creating the data frame
MeanVector = c(Mean[SelectedPoints,1:n_cases])
SdVector = c(Sd[SelectedPoints,1:n_cases])
points = rep(SelectedPoints, times = n_cases)
labels = rep(tests[1:n_cases], each = length(SelectedPoints))
dados = data.frame(HypervolumeMean = MeanVector, HypervolumeSd = SdVector, Generations = points, Labels = labels)

# Ploting the data
ggplot(dados, aes(x=Generations, y = HypervolumeMean, fill=Labels)) + 
  labs(x = "Generation", y = "Hypervolume", title = "Hypervolume comparison between CHTs (CRE21)") + 
  geom_point(aes(colour = Labels)) + geom_line(aes(colour = Labels)) + 
  xlim(0, 100) + ylim(0,1.3) +
  geom_ribbon(aes(ymin = pmax(MeanVector - SdVector,0),ymax = pmin(1.3, HypervolumeMean + HypervolumeSd), colour = Labels),alpha=0.1)

