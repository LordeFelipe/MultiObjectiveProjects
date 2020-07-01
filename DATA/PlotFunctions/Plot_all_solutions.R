# Script used to plot the all the solutions of a problem

library(MOEADr)
library(emoa)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Parameters for execution
n_objectives = 2
n_individuals = 300
n_iterations = 200
n_runs = 10

# Path to the desired problem
# ../MAZDA/         -> MAZDA Car Problem
# ../MOON/          -> Moon Landing Problem
# ../CRE/CRE21/     -> Problem suite Problem (To acess others change the number)
path = "../MAZDA/"

filename = c("200g_static100/")

allSolutions = matrix(ncol =  n_objectives+1, nrow = 0)
for(j in 1:length(filename)){
  for(i in 1:n_runs){
    # Reading the data
    newSolutions = scan(paste0(problem,filename[j],"MyArchive",i,".txt"), quiet = TRUE)
    newSolutions = matrix(newSolutions,nrow = n_individuals*n_iterations, ncol = n_objectives+1, byrow = TRUE)
    
    # Concatenating the new solutions with all solutions
    allSolutions = rbind(allSolutions, newSolutions)
    
    # Filtering all repeated solutions
    allSolutions = unique(allSolutions)
  } 
}

# Creating the labels for feasible and unfeasible
labels = allSolutions[,3]
labels = ifelse(labels,"Feasible","Unfeasible")

# Creating the data frame
data = data.frame(Objective1 = allSolutions[,1], Objective2 = allSolutions[,2], Labels = labels)

# Ploting
ggplot(data, aes(x=Objective1, y = Objective2, fill=Labels)) + geom_point(aes(colour = Labels))
