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
# ../MAZDA/         -> MAZDA Car Problem
# ../MOON/          -> Moon Landing Problem
# ../CRE/CRE21/     -> Problem suite Problem (To access others change the number)
path = "../MAZDA/"

# Name of the tests
#filename = c("200g_dynamic_alpha2_C005/","200g_dynamic_alpha2_C002/", "200g_static100/", "200g_static1/","200g_selfadapting/","200g_none/")
filename = c("200g_selfadapting/")
n_cases = length(filename)

# Options for the plotted solutions
only_feasible = FALSE
only_unfeasible = FALSE
only_non_dominated = FALSE
normalized = FALSE

# Parameters for execution
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
generation = rep(1:n_iterations,each = n_individuals)
data = data.frame(Objective1 = allSolutions[,1], Objective2 = allSolutions[,2], Labels = labels, Generation = generation)

# Ploting
p <-  ggplot(data, aes(x=Objective1, y = Objective2, fill=Labels)) +
  labs(title = "MAZDA") +
  geom_point(aes(colour = Labels), nframes = 200) +
  xlim(xlim) + 
  ylim(ylim)


p <- p + transition_states(Generation,
                      transition_length = 1,
                      state_length = 1) +
          ggtitle('Generation {frame} of {nframes}',
          subtitle = 'Current penalty: Selfadaptative')
animate(p, nframes = n_iterations)
