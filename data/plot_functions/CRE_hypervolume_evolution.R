# Function that calculates the hypervolume of every iteration of the CRE data

CRE_hypervolume_evolution <- function(case, filename, n_individuals, n_objectives, n_iterations...){
  
  NewHyper = matrix(0,ncol = n_iterations, nrow = 20)
  
  #Extracting the objective values from the files
  A = array(0,c(n_individuals*n_iterations, n_objectives+1, 20,length(filename)))
  for(i in 1:20){
    for(j in 1:length(filename)){
      YAll = scan(sprintf(paste(filename[j],"/MyArchive%d.txt",sep = ""),i), quiet = TRUE)  
      A[,,i,j] = matrix(YAll,nrow = n_individuals*n_iterations, ncol = n_objectives+1, byrow = TRUE)
    }
  }
  
  #Extracting the global max and min feasible values
  maxob = rep(-Inf,n_objectives)
  minob = rep(+Inf,n_objectives)
  for (i in 1:n_objectives){
    maxob[i] = max(A[which(A[,n_objectives+1,1,1]==1),i,1,1])
    minob[i] = min(A[which(A[,n_objectives+1,1,1]==1),i,1,1])  
  }
  
  for(i in 1:20){
    for(j in 1:length(filename)){
      for (k in 1:n_objectives){
        maxob[k] = max(maxob[k], max(A[which(A[,n_objectives+1,i,j]==1),k,i,j]))
        minob[k] = min(minob[k], min(A[which(A[,n_objectives+1,i,j]==1),k,i,j]))
      }
    }
  }
  
  for (ite in 1:20){
    #Converting the matrix to have n_iteration as a dimension
    B = array(0, c(n_individuals,n_objectives+1,n_iterations))
    for(i in 1:n_iterations){
      B[,,i] = A[((i-1)*n_individuals+1):(n_individuals*i),,ite,case]  
    }
    
    #Scaling the values using all the iterations for each objective
    Newnormalized = B
    for (i in 1:n_objectives){
      Newnormalized[,i,] = (B[,i,] - minob[i])/(maxob[i] - minob[i] + 1e-16)
    }
    
    
    for(j in 1:n_iterations){
      #No feasible solutions in the population
      if(sum(Newnormalized[,n_objectives+1,j]) == 0){
        NewHyper[ite,j] = 0
      }
      #Only one feasible solution
      else if(sum(Newnormalized[,n_objectives+1,j]) == 1){
        NewHyper[ite,j] = dominated_hypervolume(matrix(Newnormalized[which(Newnormalized[,n_objectives+1,j] == 1),1:n_objectives,j]), (rep(1.1,n_objectives)))
      }
      #Multiple feasible solutions
      else{
        NewHyper[ite,j] = dominated_hypervolume(t(Newnormalized[which(Newnormalized[,n_objectives+1,j] == 1),1:n_objectives,j]), (rep(1.1,n_objectives)))
      }
    } 
  }
  return(NewHyper)
}