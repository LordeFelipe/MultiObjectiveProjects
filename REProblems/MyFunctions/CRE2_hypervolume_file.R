CRE2_hypervolume_file <- function(filename, n_individuals, n_objectives, n_iterations...){

  #Extracting the objective values from the files
  YAll = scan(paste(getwd(), sprintf(filename), sep = "/"), quiet = TRUE)
  A = matrix(YAll,nrow = n_individuals*n_iterations, ncol = n_objectives+1, byrow = TRUE)
  
  #Extracting the max and min feasible values
  maxob1 = max(A[which(A[,3]==1),1])
  minob1 = min(A[which(A[,3]==1),1])
  maxob2 = max(A[which(A[,3]==1),2])
  minob2 = min(A[which(A[,3]==1),2])
  
  B = array(0, c(n_individuals,n_objectives+1,n_iterations))
  for(i in 1:n_iterations){
    B[,,i] = A[((i-1)*n_individuals+1):(n_individuals*i),]  
  }
  
  #Normalizing the values using all the iterations
  Newnormalized = B
  Newnormalized[,1,] = (B[,1,] - maxob1)/(minob1 - maxob1)
  Newnormalized[,2,] = (B[,2,] - maxob2)/(minob2 - maxob2)
  
  NewHyper = matrix(0,ncol = n_iterations, nrow = 1)
  for(j in 1:n_iterations){
    #No feasible solutions in the population
    if(sum(Newnormalized[,3,j]) == 0){
      NewHyper[j] = 0
    }
    #Only one feasible solution
    else if(sum(Newnormalized[,3,j]) == 1){
      NewHyper[j] = dominated_hypervolume(matrix(Newnormalized[which(Newnormalized[,3,j] == 1),1:2,j]), (c(1.1,1.1)))
    }
    #Multiple feasible solutions
    else{
      NewHyper[j] = dominated_hypervolume(t(Newnormalized[which(Newnormalized[,3,j] == 1),1:2,j]), (c(1.1,1.1)))
    }
  } 
  return(NewHyper)
}