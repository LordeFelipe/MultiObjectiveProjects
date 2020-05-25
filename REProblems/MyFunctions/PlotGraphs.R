library(MOEADr)
library(emoa)
library(ggplot2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

debugSource("../MyFunctions/CREProblems_hypervolume_evolution.R")
debugSource("../MyFunctions/CRE3_hypervolume_files.R")
debugSource("../MyFunctions/CRE2_hypervolume_files.R")

tests = c("none","static1","static100","selfadapting","dynamic_alpha2_C005","dynamic_alpha2_C002")
#tests = c("static1","static100")
filenames = paste0("../DATA/CRE51/",tests)

n_objectives = 5
n_individuals = 210
n_iterations = 100
n_cases = length(tests)

NewHyper = matrix(0, nrow = 20, ncol = n_iterations)
Mean = matrix(0, nrow = n_iterations, ncol = n_cases)
Median = matrix(0, nrow = n_iterations, ncol = n_cases)
Sd = matrix(0, nrow = n_iterations, ncol = n_cases)
dados = data.frame


for(nfile in 1:n_cases){
  NewHyper = CREProblems_hypervolume_evolution(case = nfile, filename = filenames, n_individuals = n_individuals, n_iterations = n_iterations, n_objectives = n_objectives)
  for(i in 1:n_iterations){
    Median[i,nfile] = median(NewHyper[,i])
    Mean[i,nfile] = mean(NewHyper[,i])
    Sd[i,nfile] = sd(NewHyper[,i])
  }
}

SelectedPoints = c(1:100)

MeanVector = c(Mean[SelectedPoints,1:n_cases])
SdVector = c(Sd[SelectedPoints,1:n_cases])
points = rep(SelectedPoints, times = n_cases)
labels = rep(tests[1:n_cases], each = length(SelectedPoints))
dados = data.frame(HypervolumeMean = MeanVector, HypervolumeSd = SdVector, Generations = points, Labels = labels)
ggplot(dados, aes(x=Generations, y = HypervolumeMean, fill=Labels)) + 
  labs(x = "Generation", y = "Hypervolume", title = "Hypervolume comparison between CHTs (CRE51)") + 
  geom_point(aes(colour = Labels)) + geom_line(aes(colour = Labels)) + 
  xlim(0, 100) + ylim(0,1.3) +
  geom_ribbon(aes(ymin = pmax(MeanVector - SdVector,0),ymax =HypervolumeMean + HypervolumeSd, colour = Labels),alpha=0.1)

