CRE3_hypervolume_files <- function(case, filename, n_individuals, n_objectives, n_iterations...){

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
  maxob1 = max(A[which(A[,4,1,1]==1),1,1,1])
  minob1 = min(A[which(A[,4,1,1]==1),1,1,1])
  maxob2 = max(A[which(A[,4,1,1]==1),2,1,1])
  minob2 = min(A[which(A[,4,1,1]==1),2,1,1])
  maxob3 = max(A[which(A[,4,1,1]==1),3,1,1])
  minob3 = min(A[which(A[,4,1,1]==1),3,1,1])
  for(i in 1:20){
    for(j in 1:length(filename)){
      maxob1 = ifelse(maxob1 > max(A[which(A[,4,i,j]==1),1,i,j]), maxob1, max(A[which(A[,4,i,j]==1),1,i,j]))
      minob1 = ifelse(minob1 < min(A[which(A[,4,i,j]==1),1,i,j]), minob1, min(A[which(A[,4,i,j]==1),1,i,j]))
    
      maxob2 = ifelse(maxob2 > max(A[which(A[,4,i,j]==1),2,i,j]), maxob2, max(A[which(A[,4,i,j]==1),2,i,j]))
      minob2 = ifelse(minob2 < min(A[which(A[,4,i,j]==1),2,i,j]), minob2, min(A[which(A[,4,i,j]==1),2,i,j]))
      
      maxob3 = ifelse(maxob3 > max(A[which(A[,4,i,j]==1),3,i,j]), maxob3, max(A[which(A[,4,i,j]==1),3,i,j]))
      minob3 = ifelse(minob3 < min(A[which(A[,4,i,j]==1),3,i,j]), minob3, min(A[which(A[,4,i,j]==1),3,i,j]))
    }
  }
  for (ite in 1:20){
    #Converting the matrix to have n_iteration as a dimension
    B = array(0, c(n_individuals,n_objectives+1,n_iterations))
    for(i in 1:n_iterations){
      B[,,i] = A[((i-1)*n_individuals+1):(n_individuals*i),,ite,case]  
    }
    
    #Normalizing the values using all the iterations
    Newnormalized = B
    Newnormalized[,1,] = (B[,1,] - minob1)/(maxob1 - minob1)
    Newnormalized[,2,] = (B[,2,] - minob2)/(maxob2 - minob2)
    Newnormalized[,3,] = (B[,3,] - minob3)/(maxob3 - minob3)
    
    for(j in 1:n_iterations){
      #No feasible solutions in the population
      if(sum(Newnormalized[,4,j]) == 0){
        NewHyper[ite,j] = 0
      }
      #Only one feasible solution
      else if(sum(Newnormalized[,4,j]) == 1){
        NewHyper[ite,j] = dominated_hypervolume(matrix(Newnormalized[which(Newnormalized[,4,j] == 1),1:3,j]), (c(1.1,1.1,1.1)))
      }
      #Multiple feasible solutions
      else{
        NewHyper[ite,j] = dominated_hypervolume(t(Newnormalized[which(Newnormalized[,4,j] == 1),1:3,j]), (c(1.1,1.1,1.1)))
      }
    } 
  }
  return(NewHyper)
}