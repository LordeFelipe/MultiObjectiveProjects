# MultiObjectiveProjects
A repository containing the implementation of a variety of Multi-Objective Problems

## Problems
#### MAZDA Car Problem

#### RE problem suite

## How to Execute the tests

1. Acess the folder of the desired problem.
2. Set the working directory to the source file location
3. On the top of the code, the number of generations and population size can be changed
4. All the components can be changed further down the code
5. Just run the code

##### Observations

1. When the execution starts, 20 files are created to store the solutions from each generation
2. I still need to update some of the RE problems to be independent of the executable file in their folder. They still use it to calculate the constraints. For problems CR22, CR31, CR32 and CR52, run their makefile with make before running the R script.
3. For the problems with 3+ objectives, the population is set to a number a lot lower than 300 which is because of the way SLD decomposition method works. For example, in the 3 objective problems 325 subproblems are created with 25 as the input. This is temporary and i will probably switch it to Unifom Design.
4. To plot the hypervolume evolution graph, use the script PlotGraphs.R (Oh God, what a horrible name). Put the directory path on the filenames variable and the name of the folders containing the tests with the 20 files in the tests vector.
5. For some reason that I still don't understand, the PlotGraphs.R function for the MAZDA car problem is in the problem's folder and the one of the problem suite is in REProblems/MyFunctions folder. Write this just made me realize that I need to organizate all this urgently. I swear I will do this in June when i get some time.


