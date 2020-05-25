source("MAZDA_hypervolume_file.R")
library(emoa)
library(ggplot2)

tests = c("CorrectScaled_static100","CorrectScaled_static1","CorrectScaled_dynamic_alpha2_C002","CorrectScaled_dynamic_alpha2_C005","CorrectScaled_MultiStaged_beta05_stages4","CorrectScaled_SelfAdapting")
n_objectives = 2
n_individuals = 300
n_iterations = 100
n_cases = length(tests)
filenames = paste0("DATA/INCUBENTSOLUTIONS/")

NewHyper = matrix(0, nrow = 20, ncol = n_iterations)
Mean = matrix(0, nrow = n_iterations, ncol = n_cases)
Median = matrix(0, nrow = n_iterations, ncol = n_cases)
Sd = matrix(0, nrow = n_iterations, ncol = n_cases)
dados = data.frame



for(nfile in 1:n_cases){
  for(i in 1:20){
    NewHyper[i,] = MAZDA_hypervolume_file(filename = sprintf(paste(filenames,tests[nfile],"/MyArchive%d.txt",sep = ""),i), n_individuals = n_individuals, n_iterations = n_iterations, n_objectives = n_objectives)
  }
  
  for(i in 1:n_iterations){
    Median[i,nfile] = median(NewHyper[,i])
    Mean[i,nfile] = mean(NewHyper[,i])
    Sd[i,nfile] = sd(NewHyper[,i])
  }
}

SelectedPoints = c(1,5*c(1:20))

MeanVector = c(Mean[SelectedPoints,1:n_cases])
SdVector = c(Sd[SelectedPoints,1:n_cases])
points = rep(c(1,5*c(1:20)), times = n_cases)
labels = rep(tests[1:n_cases], each = 21)
dados = data.frame(HypervolumeMean = MeanVector, HypervolumeSd = SdVector, Generations = points, Labels = labels)
ggplot(dados, aes(x=Generations, y = HypervolumeMean, fill=Labels)) + 
  labs(x = "Generation", y = "Hypervolume", title = "Hypervolume comparison between CHTs") + 
  geom_point(aes(colour = Labels)) + 
  geom_line(aes(colour = Labels)) + 
  xlim(0, 100) + ylim(0,0.12) +
  geom_ribbon(aes(ymin = pmax(MeanVector - SdVector,0),ymax =HypervolumeMean + HypervolumeSd, colour = Labels),alpha=0.1)

