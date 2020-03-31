library(MOEADr)
library(emoa)

#######################################################################################
updt_standardx <- function(X, Xt, Y, Yt, V, Vt, sel.indx, B, ...){
  # Solution x_i^{t+1} will receive the best solution from the set:
  # ${x_i^t, {v_j^t \forall j \in N(i)}} | w_i$
  # where $v_j^t$ is the j-th 'offspring' candidate solution, N(i) is the
  # neighborhood of i, and $w_i$ is the i-th weight vector.
  
  # Get best selection index for each neighborhood
  std.sel.indx <- sel.indx[, 1]
  
  # Function for returning the selected solution (variable or objectives space)
  # for a subproblem:
  # - i: subproblem index
  # - sel.indx: vector of selection indices (see above)
  # - XY: matrix of candidate solutions (in variable or objective space)
  # - XYt: matrix of incumbent solutions (in variable or objective space)
  # - B: matrix of neighborhoods
  do.update <- function(i, sel.indx, XY, XYt, B){
    if (sel.indx[i] > ncol(B)) return(XYt[i, ]) # last row = incumbent solution
    else return(XY[B[i, sel.indx[i]], ])
  }
  
  # Update matrix of candidate solutions
  Xnext <- t(vapply(X         = 1:nrow(X),
                    FUN       = do.update,
                    FUN.VALUE = numeric(ncol(X)),
                    sel.indx  = std.sel.indx,
                    XY        = X,
                    XYt       = Xt,
                    B         = B,
                    USE.NAMES = FALSE))
  
  # Update matrix of function values
  Ynext <- t(vapply(X         = 1:nrow(Y),
                    FUN       = do.update,
                    FUN.VALUE = numeric(ncol(Y)),
                    sel.indx  = std.sel.indx,
                    XY        = Y,
                    XYt       = Yt,
                    B         = B,
                    USE.NAMES = FALSE))
  
  
  # Update list of constraint values
  if(is.null(V)){
    Vnext <- NULL
  } else{
    Vnext <- list(Cmatrix = NULL, Vmatrix = NULL, v = NULL)
    
    ## 1: Cmatrix
    Vnext$Cmatrix <- t(vapply(X         = 1:nrow(V$Cmatrix),
                              FUN       = do.update,
                              FUN.VALUE = numeric(ncol(V$Cmatrix)),
                              sel.indx  = std.sel.indx,
                              XY        = V$Cmatrix,
                              XYt       = Vt$Cmatrix,
                              B         = B,
                              USE.NAMES = FALSE))
    ## 2: Vmatrix
    Vnext$Vmatrix <- t(vapply(X         = 1:nrow(V$Vmatrix),
                              FUN       = do.update,
                              FUN.VALUE = numeric(ncol(V$Vmatrix)),
                              sel.indx  = std.sel.indx,
                              XY        = V$Vmatrix,
                              XYt       = Vt$Vmatrix,
                              B         = B,
                              USE.NAMES = FALSE))
    
    ## 3: v
    Vnext$v <- rowSums(Vnext$Vmatrix)
  }
  normalized = Ynext
  normalized[,1] = (Ynext[,1] - max(Ynext[,1]))/(min(Ynext[,1]) - max(Ynext[,1]))
  normalized[,2] = (Ynext[,2] - max(Ynext[,2]))/(min(Ynext[,2]) - max(Ynext[,2]))
  
  if(max(Vnext$v == 0) == 0){
    cat(0)
    cat("\n")
  }
  
  #At least one feasible solution
  else{
    #Only use the solutions which violates none of the constraints
    cat(dominated_hypervolume(t(normalized[which(Vnext$v == 0),]), (c(1.1,1.1))))
    cat("\n")
  }
  
  
  # Output
  return(list(X = Xnext,
              Y = Ynext,
              V = Vnext))
}
#######################################################################################

n_individuals = 100
n_iterations = 50

#Creating Variable Bounds
minimum = c(0.0001, 0.0001, 1.0)
maximum = c(100.0, 100.0, 3.0)

#Objective1
Objective1 <- function(X){
  
  write(t(X),file = paste(getwd(), "/pop_vars_eval.txt", sep="/"), ncolumns = 3, sep = " ")
  system("./example", ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "pop_vars_obj.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 2, byrow = TRUE)
  
  obj1 = matrix(objectives[,1], ncol = 1)
  obj1
}

#Objective2
Objective2 <- function(X){
  
  write(t(X),file = paste(getwd(), "/pop_vars_eval.txt", sep="/"), ncolumns = 3, sep = " ")
  system("./example", ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "pop_vars_obj.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 2, byrow = TRUE)
  
  obj2 = matrix(objectives[,2], ncol = 1)
  obj2
}

#Definition of the problem
problem.cr21 <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(Objective1(X), Objective2(X)) }
  ))
}

problem.1 <- list(name       = "problem.cr21",  # Function that executes the MOP
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = 2)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "SLD",H = n_individuals - 1)

## 2 - Neighbors
neighbors <- list(name    = "lambda",
                  T       = floor(n_individuals*0.2), #Size of the neighborhood
                  delta.p = 1) #Probability of using the neighborhood

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- list(name = "standardx", UseArchive = FALSE)

## 5 - Scaling
scaling <- list(name = "simple")

## 6 - Stop criterion
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = n_iterations))

## 7 - Variation Operators
variation <- list(list(name  = "sbx",
                       etax  = 20, pc = 0.7),
                  list(name  = "polymut",
                       etam  = 20, pm = 0.3 ),
                  list(name  = "truncate"))

## 8 - Show
showpars  <- list(show.iters = "dots",
                  showevery  = 20)

## 9 - Constraint
my_constraints <- function(X)
{
  nv <- 3 # number of variables
  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = 2 * nv + 3) 
  
  colnames(Cmatrix) <- c(paste0("x",
                                rep(1:nv, times = 2),
                                rep(c("min","max"), each = nv)),
                         rep(c("g1"), each = 3))
  
  # Box limits of the feasible space
  Xmin <- matrix(minimum, ncol = 3, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(maximum, ncol = 3, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  g1 <- function(X){
    write(t(X),file = paste(getwd(), "/pop_vars_eval.txt", sep="/"), ncolumns = 3, sep = " ")
    system("./example", ignore.stdout = TRUE)
    constraints <- scan(paste(getwd(), "pop_vars_cons.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = 3, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + 3)] <- g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + 3)] <- pmax(Vmatrix[, 1:(2 * nv + 3)], 0)        # inequality constraints

  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}
constraint<- list(name = "penalty", beta = 0.5)


## 10 - Execution

hyper = rep(0,20)
hyperteste = rep(0,20)
besthyper = -1
for (i in 1:20){
  results <- moead(problem  = problem.1,
                   decomp = decomp,
                   neighbors = neighbors,
                   aggfun = aggfun,
                   scaling = scaling,
                   constraint = constraint,
                   stopcrit = stopcrit,
                   update = update,
                   variation = variation,
                   showpars = showpars,
                   seed     = floor(runif(1)*1000))
  #Normalizing Objective Funtions
  normalized = results$Y
  normalized[,1] = (results$Y[,1] - results$ideal[1])/(results$nadir[1] - results$ideal[1])
  normalized[,2] = (results$Y[,2] - results$ideal[2])/(results$nadir[2] - results$ideal[2])
  #plot(normalized[which(results$V$v == 0),1],normalized[which(results$V$v == 0),2])
  
  
  #Calculate the hypervolume only with feasible points
  #If there is no feasible solutions, the hypervolume is 1
  if(max(results$V$v == 0) == 0){
    hyper[i] = 0
  }
  
  #At least one feasible solution
  else{
    #Only use the solutions which violates none of the constraints
    hyper[i] = dominated_hypervolume(t(normalized[which(results$V$v == 0),]), (c(1.1,1.1)))
    cat("Iteration: ", i,"Hyper: ", hyper[i])
  }
  
  #Saves the best result
  if(hyper[i] > besthyper){
    bestresults = results
    besthyper = hyper[i]
  }
}
