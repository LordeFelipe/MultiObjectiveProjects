# Script used to plot the hypervolume evolution of different tests
# in a specific problem. The name of the test 
# should be specified in the "tests" variable. 

library(MOEADr)
library(emoa)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

debugSource("MAZDA_hypervolume_evolution.R")
debugSource("CRE_hypervolume_evolution.R")
debugSource("MOON_hypervolume_evolution.R")

# Path to the desired problem
# ../mazda/populations/200_generations/         -> MAZDA Car Problem
# ../moon/populations/200_generations/          -> Moon Landing Problem
# ../CRE/CRE21/     -> Problem suite Problem (To acess others change the number)
path = "../mazda/populations/200_generations/"

# Write if the problem is MAZDA, MOON or CRE
problem = "MAZDA"

# Names of the tests and their path
all_files = list.files(path)
tests = all_files[13]
n_cases = length(tests)

filenames = paste0(path,tests)

# Parameters for execution
n_objectives = 2
n_individuals = 300
n_iterations = 200
n_runs = 10

# Chosen generations to appear in the x axis plot
SelectedPoints = c(1:n_iterations)

# Initialization of some variables
NewHyper = matrix(0, nrow = n_runs, ncol = n_iterations)
Mean = matrix(0, nrow = n_iterations, ncol = n_cases)
Median = matrix(0, nrow = n_iterations, ncol = n_cases)
Sd = matrix(0, nrow = n_iterations, ncol = n_cases)
dados = data.frame

# Calculating the mean and standard deviation of each iteration of all the tests
for(nfile in 1:n_cases){
  
  if(problem == "MAZDA"){
    for(i in 1:n_runs){
      NewHyper[i,] = MAZDA_hypervolume_evolution(filename = sprintf(paste(filenames[nfile],"/MyArchive%d.txt",sep = ""),i), n_individuals = n_individuals, n_iterations = n_iterations, n_objectives = n_objectives)  
    }
  }
  else if(problem == "CRE"){
    NewHyper = CRE_hypervolume_evolution(case = nfile, filename = filenames, n_individuals = n_individuals, n_iterations = n_iterations, n_objectives = n_objectives)  
  }
  else if(problem == "MOON"){
    for(i in 1:n_runs){
      NewHyper[i,] = MOON_hypervolume_evolution(filename = sprintf(paste(filenames[nfile],"/MyArchive%d.txt",sep = ""),i), n_individuals = n_individuals, n_iterations = n_iterations, n_objectives = n_objectives)  
    }
  }
  
  for(i in 1:n_iterations){
    Median[i,nfile] = median(NewHyper[,i])
    Mean[i,nfile] = mean(NewHyper[,i])
    Sd[i,nfile] = sd(NewHyper[,i])
  }
}

# Creating the labels
labels = gsub("^.*?200g_","",tests[1:n_cases])
labels = gsub("_"," ",labels)

# Creating the data frame
MeanVector = c(Mean[SelectedPoints,1:n_cases])
SdVector = c(Sd[SelectedPoints,1:n_cases])
points = rep(SelectedPoints, times = n_cases)
labels = rep(labels, each = length(SelectedPoints))
dados = data.frame(HypervolumeMean = MeanVector, HypervolumeSd = SdVector, Generations = points, Labels = labels)

# Ploting the data
ggplot(dados, aes(x=Generations, y = HypervolumeMean, fill=Labels)) + 
  labs(x = "Generation", y = "Hypervolume", title = "Hypervolume comparison between CHTs") + 
  geom_point(aes(colour = Labels)) + geom_line(aes(colour = Labels)) + 
  xlim(0, n_iterations) +
  ylim(0, 0.15) + 
  geom_ribbon(aes(ymin = pmax(MeanVector - SdVector,0),ymax = pmin(1.1, HypervolumeMean + HypervolumeSd), colour = Labels),alpha=0.1)

ggsave(paste0(tests,".jpg"), device = "jpg", width = 9, height = 6)

