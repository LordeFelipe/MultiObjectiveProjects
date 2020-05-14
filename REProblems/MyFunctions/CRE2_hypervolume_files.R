filename = "DATA/P2/"


CRE2_hypervolume_files <- function(filename, n_individuals, n_objectives, n_iterations...){

  #Extracting the objective values from the files
  A = array(0,c(n_individuals*n_iterations, n_objectives+1, 20))
  for(i in 1:20){
    YAll = scan(sprintf(paste("DATA/P2/","/MyArchive%d.txt",sep = ""),i), quiet = TRUE)  
    A[,,i] = matrix(YAll,nrow = n_individuals*n_iterations, ncol = n_objectives+1, byrow = TRUE)
  }
  
  #Extracting the global max and min feasible values
  maxob1 = max(A[which(A[,3,1]==1),1,1])
  minob1 = min(A[which(A[,3,1]==1),1,1])
  maxob2 = max(A[which(A[,3,1]==1),2,1])
  minob2 = min(A[which(A[,3,1]==1),2,1])
  for(i in 2:20){
    maxob1 = ifelse(maxob1 > max(A[which(A[,3,i]==1),1,i]), maxob1, max(A[which(A[,3,i]==1),1,i]))
    minob1 = ifelse(minob1 < min(A[which(A[,3,i]==1),1,i]), minob1, min(A[which(A[,3,i]==1),1,i]))
    
    maxob2 = ifelse(maxob2 > max(A[which(A[,3,i]==1),2,i]), maxob2, max(A[which(A[,3,i]==1),2,i]))
    minob2 = ifelse(minob2 < min(A[which(A[,3,i]==1),2,i]), minob2, min(A[which(A[,3,i]==1),2,i]))
  }
  for (ite in 1:20){
    #Converting the matrix to have n_iteration as a dimension
    B = array(0, c(n_individuals,n_objectives+1,n_iterations))
    for(i in 1:n_iterations){
      B[,,i] = A[((i-1)*n_individuals+1):(n_individuals*i),,ite]  
    }
    
    #Normalizing the values using all the iterations
    Newnormalized = B
    Newnormalized[,1,] = (B[,1,] - minob1)/(maxob1 - minob1)
    Newnormalized[,2,] = (B[,2,] - minob2)/(maxob2 - minob2)
    
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
    plot(c(1:100),NewHyper)
  }
  return(NewHyper)
}