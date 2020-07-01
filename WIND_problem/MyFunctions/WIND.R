
#Function to normalize the parameters acording to their max and min
Normalize <- function(X, min, max){
  (X - min) / (max - min)
}

#Objecive Function for annual power generation
AnnualPowerGen <- function(X){
  X = Normalize(X, min, max)
  write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = "\t")
  system("python windturbine_mop.py Evaluate")
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = n_objectives, byrow = TRUE)
  
  gen = matrix(objectives[,1], ncol = 1)
  gen
}

#Objecive Function for Average Annual Cost
AverageAnnualCost <- function(X){
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = n_objectives, byrow = TRUE)
  
  cost = matrix(objectives[,2], ncol = 1)
  cost
}

#Objecive Function for Tower Base Cost
TowerbaseCost <- function(X){
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = n_objectives, byrow = TRUE)
  
  base = matrix(objectives[,3], ncol = 1)
  base
}

#Objecive Function for Blade Tip Speed
BladeTipSpeed <- function(X){
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = n_objectives, byrow = TRUE)
  
  speed = matrix(objectives[,4], ncol = 1)
  speed
}

#Objecive Function for Fatigue Damage
FatigueDamage <- function(X){
  objectives <- scan(paste(getwd(), "Evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = n_objectives, byrow = TRUE)
  
  damage = matrix(objectives[,5], ncol = 1)
  damage
}

#Definition of the problem
problem.wind <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(AnnualPowerGen(X), AverageAnnualCost(X), TowerbaseCost(X), BladeTipSpeed(X), FatigueDamage(X) ) }
  ))
}

## 9 - Constraint
my_constraints <- function(X)
{
  nv <- n_variables # number of variables
  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = 2 * nv + n_constraints) 
  
  colnames(Cmatrix) <- c(paste0("x",
                                rep(1:nv, times = 2),
                                rep(c("min","max"), each = nv)),
                         rep(c("g1"), each = n_constraints))
  
  # Box limits of the feasible space
  Xmin <- matrix(minmax[,1], ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(minmax[,2], ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  g1 <- function(X){
    write(X,file = paste(getwd(), "Evaluate/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = "\t")
    system("python windturbine_mop.py Evaluate")
    constraints <- scan(paste(getwd(), "Evaluate/pop_cons_eval.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = n_constraints, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- -g1(X) # Constraints need to be greater than zero
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)        # inequality constraints
  
  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}