# Script used to 3d plot all the solutions of a problem in the
# objectives space. Select the desired problem in the path variable
# and the desired tests in the filename vector

library(MOEADr)
library(emoa)
library(rgl)
library(magic)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#---------------------------------PARAMETERS---------------------------------#

path = "../moon/populations/200_generations/"

# Name of the tests
#filename = c("200g_dynamic_alpha2_C005/","200g_dynamic_alpha2_C002/", "200g_static100/", "200g_static1/","200g_selfadapting/","200g_none/")
filename = c("static100/")
n_cases = length(filename)

# Options for the plotted solutions
only_feasible = FALSE
only_unfeasible = FALSE

# Parameters for execution
n_objectives = 3
n_individuals = 325
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
  allSolutions = allSolutions[which(allSolutions[,n_objectives+1] == 1),]
} else if (only_unfeasible){
  allSolutions = allSolutions[which(allSolutions[,n_objectives+1] == 0),]
}


# Creating the labels for feasible and unfeasible
labels = allSolutions[,n_objectives+1]
labels = ifelse(labels,"#00FF00", "#FF0000")

# Creating the data frame
data = data.frame(Objective1 = allSolutions[,1], 
                  Objective2 = allSolutions[,2], 
                  Objective3 = allSolutions[,3], 
                  Labels = labels)

# Ploting

plot3d( data$Objective1, data$Objective2, data$Objective3,
        xlab = "number of continuous shaded days",
        ylab = "total communication time",
        zlab = "tilt angle",
        col = data$Labels, type = "s", radius = .01 )


