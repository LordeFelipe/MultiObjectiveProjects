# Script used to plot all the solutions of a problem in the
# objectives space. Select the desired problem in the path variable
# and the desired tests in the filename vector

library(MOEADr)
library(emoa)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#---------------------------------PARAMETERS---------------------------------#

# 1. Select the path to the desired problem
# ../data_output/MAZDA/         -> MAZDA Car Problem
# ../data_output/MOON/          -> Moon Landing Problem
# ../data_output/CRE21/         -> Problem suite Problem (To access others change the number)
path = "../data_output/MAZDA/"

# 2. Select the CHTs
filename = c("Self-adaptive/")
n_cases = length(filename)

# 3. Select the options for the plotted solutions
only_feasible = FALSE
only_unfeasible = FALSE
normalized = FALSE

# 4. Set the parameters for execution
n_objectives = 2
n_individuals = 300
n_iterations = 200
n_runs = 10

#---------------------------------CODE---------------------------------#

# Initializing the solutions matrix
allSolutions = matrix(ncol =  n_objectives+1, nrow = 0)

for(j in 1:n_cases){
  for(i in 1:n_runs){
    # Reading the data
    newSolutions = scan(paste0(path,filename[j],"MyArchive",i,".txt"), quiet = TRUE)
    newSolutions = matrix(newSolutions,nrow = n_individuals*n_iterations, ncol = n_objectives+1, byrow = TRUE)
    
    # Concatenating the new solutions with all solutions
    allSolutions = rbind(allSolutions, newSolutions)
    
    # Filtering all repeated solutions
    allSolutions = unique(allSolutions)
  } 
}

# Applying filters to the solutions if necessary
if (only_feasible){
  allSolutions = allSolutions[which(allSolutions[,3] == 1),]
} else if (only_unfeasible){
  allSolutions = allSolutions[which(allSolutions[,3] == 0),]
}


if(normalized){
  allSolutions[,1] = allSolutions[,1]/74
  allSolutions[,2] = allSolutions[,2] - 2
  
  xlim = c(-1,0)
  ylim = c(0,1.8)
}else{
  xlim = c(-74,0)
  ylim = c(2,3.8)
}

# Creating the labels for feasible and unfeasible
labels = allSolutions[,3]
labels = ifelse(labels,"Feasible","Unfeasible")

# Creating the data frame
data = data.frame(Objective1 = allSolutions[,1], Objective2 = allSolutions[,2], Labels = labels)

# Ploting
ggplot(data, aes(x=Objective1, y = Objective2, fill=Labels)) +
      labs(title = "MAZDA all solutions") +
      geom_jitter(aes(colour = Labels)) +
      xlim(xlim) + 
      ylim(ylim)


