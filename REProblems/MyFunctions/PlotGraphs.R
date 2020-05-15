debugSource("../MyFunctions/CRE2_hypervolume_files.R")

tests = c("static025","static05","static1","static2","static100","selfadapting")
filenames = paste0("../MyFunctions/DATA/CRE21/",tests)

n_objectives = 2
n_individuals = 300
n_iterations = 100
n_cases = length(tests)

NewHyper = matrix(0, nrow = 20, ncol = n_iterations)
Mean = matrix(0, nrow = n_iterations, ncol = n_cases)
Median = matrix(0, nrow = n_iterations, ncol = n_cases)
Sd = matrix(0, nrow = n_iterations, ncol = n_cases)
dados = data.frame


for(nfile in 1:n_cases){
  NewHyper = CRE2_hypervolume_files(case = nfile, filename = filenames, n_individuals = n_individuals, n_iterations = n_iterations, n_objectives = n_objectives)
  
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
ggplot(dados, aes(x=Generations, y = HypervolumeMean, fill=Labels)) + labs(x = "Generation", y = "Hypervolume", title = "Dynamic and Static Penalty Hypervolume Evolution ") + geom_point(aes(colour = Labels)) + geom_line(aes(colour = Labels)) #+ geom_ribbon(aes(ymin = HypervolumeMean - HypervolumeSd,ymax =HypervolumeMean + HypervolumeSd, colour = Labels),alpha=0.1)

