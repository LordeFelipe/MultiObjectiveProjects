# Script used to plot all the solutions of a problem in the
# objectives space. Select the desired problem in the path variable
# and the desired tests in the filename vector

library(MOEADr)
library(emoa)
library(ggplot2)
library(gganimate)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#---------------------------------PARAMETERS---------------------------------#

# Path to the desired problem
# ../data_output/MAZDA/         -> MAZDA Car Problem
# ../data_output/MOON/          -> Moon Landing Problem
# ../data_output/CRE21/         -> Problem suite Problem (To access others change the number)
path = "../data_output/CRE21/"

# Name of the tests
filename = c("dynamicC005alpha2/")
n_cases = length(filename)

# Options for the plotted solutions
only_feasible = FALSE
only_unfeasible = FALSE
only_non_dominated = FALSE

# Parameters for execution
n_objectives = 2
n_individuals = 300
n_iterations = 100
n_runs = 1

xlim = c(0,1)
ylim = c(0,100000)

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
  } 
}

# Applying filters to the solutions if necessary
if (only_feasible){
  allSolutions = allSolutions[which(allSolutions[,3] == 1),]
} else if (only_unfeasible){
  allSolutions = allSolutions[which(allSolutions[,3] == 0),]
}

if(only_non_dominated){
  allSolutions = t(nondominated_points(t(allSolutions[,1:3])))
}


# Creating the labels for feasible and unfeasible
labels = allSolutions[,3]
labels = ifelse(labels,"Feasible","Unfeasible")

# Creating the data frame
generation = rep(1:n_iterations,each = n_individuals)
data = data.frame(Objective1 = allSolutions[,1], Objective2 = allSolutions[,2], Labels = labels, Generation = generation)

# Ploting
p <-  ggplot(data, aes(x=Objective1, y = Objective2, fill=Labels)) +
  labs(title = "CRE21") +
  geom_point(aes(colour = Labels)) +
  xlim(xlim) + 
  ylim(ylim)


p <- p + transition_states(Generation,
                      transition_length = 1,
                      state_length = 1) +
          ggtitle('Generation {frame} of {nframes}',
          subtitle = 'Current penalty: {(0.05*frame)^2}')
animate(p, nframes = n_iterations)
