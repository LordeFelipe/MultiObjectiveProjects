MOON_hypervolume_evolution <- function(filename, n_individuals, n_objectives, n_iterations...){

  #Extracting the objective values from the files
  YAll = scan(paste(getwd(), sprintf(filename,i), sep = "/"), quiet = TRUE)
  A = matrix(YAll,nrow = n_individuals*n_iterations, ncol = n_objectives+1, byrow = TRUE)
  
  B = array(0, c(n_individuals,n_objectives+1,n_iterations))
  for(i in 1:n_iterations){
    B[,,i] = A[((i-1)*n_individuals+1):(n_individuals*i),]  
  }
  
  NewHyper = matrix(0,ncol = n_iterations, nrow = 1)
  Newnormalized = B
  for(j in 1:n_iterations){
    #No feasible solutions in the population
    if(sum(Newnormalized[,4,j]) == 0){
      NewHyper[j] = 0
    }
    #Only one feasible solution
    else if(sum(Newnormalized[,4,j]) == 1){
      NewHyper[j] = dominated_hypervolume(matrix(Newnormalized[which(Newnormalized[,4,j] == 1),1:3,j]), (c(1,0,1)))
    }
    #Multiple feasible solutions
    else{
      NewHyper[j] = dominated_hypervolume(t(Newnormalized[which(Newnormalized[,4,j] == 1),1:3,j]), (c(1,0,1)))
    }
  } 
  return(NewHyper)
}
