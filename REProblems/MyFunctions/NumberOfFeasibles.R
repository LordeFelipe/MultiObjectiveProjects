library(MOEADr)
library(emoa)
library(ggplot2)

#tests = c("static1","static2","static100","selfadapting","none","dynamic_alpha2_C005","dynamic_alpha2_C002")
tests = c("none","static1")
path = "../DATA/CRE21/"

n_objectives = 2
n_individuals = 300
n_iterations = 100
n_cases = length(tests)

NumberOfFeasibles = matrix(0, nrow = 20, ncol = n_iterations)
MeanFeasible = matrix(0, nrow = n_iterations, ncol = n_cases)
SdFeasible = matrix(0, nrow = n_iterations, ncol = n_cases)

for(nfile in 1:n_cases){
  for(i in 1:20){
    #Extracting the objective values from the files
    YAll = scan(paste(getwd(), filename = sprintf(paste(path ,tests[nfile],"/MyArchive%d.txt",sep = ""),i), sep = "/"), quiet = TRUE)
    A = matrix(YAll,nrow = n_individuals*n_iterations, ncol = n_objectives+1, byrow = TRUE)
    
    B = array(0, c(n_individuals,n_objectives+1,n_iterations))
    for(j in 1:n_iterations){
      B[,,j] = A[((j-1)*n_individuals+1):(n_individuals*j),]  
    }
  
    NumberOfFeasibles[i,] = colSums(B[,n_objectives+1,])
  }
  for(i in 1:n_iterations){
    MeanFeasible[i,nfile] = mean(NumberOfFeasibles[,i])
    SdFeasible[i,nfile] = sd(NumberOfFeasibles[,i])
  }
}


SelectedPoints = c(1,5*c(1:20))

MeanVector = c(MeanFeasible[SelectedPoints,1:n_cases])
SdVector = c(SdFeasible[SelectedPoints,1:n_cases])
points = rep(c(1,5*c(1:20)), times = n_cases)
labels = rep(tests[1:n_cases], each = 21)

dados = data.frame(FeasibleMean = MeanVector, FeasibleSd = SdVector, Generations = points, Labels = labels)
ggplot(dados, aes(x=Generations, y = FeasibleMean)) + labs(x = "Generation", y = "Number of Feasible Solutions", title = "Dynamic Penalty Number of Feasible Solutions Evolution") + geom_point(aes(colour = Labels)) + geom_line(aes(colour = Labels)) + geom_ribbon(aes(ymin = FeasibleMean - FeasibleSd,ymax =FeasibleMean + FeasibleSd, colour = Labels),alpha=0.1)
