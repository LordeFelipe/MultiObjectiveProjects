suppressPackageStartupMessages(library(irace))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(smoof))
suppressPackageStartupMessages(library(MOEADr))

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
debugSource("../tunning/constraints/C1-DTLZ1.R")
debugSource("../tunning/constraints/C1-DTLZ3.R")
debugSource("../tunning/constraints/C2-DTLZ2.R")

constraint_C1DTLZ3 <- function(X){
  
  Y = DTLZ(X)
  n_objs = ncol(Y)
  
  if(n_objs <= 2){
    r = 6
  } else if(n_objs >= 3 && n_objs < 5){
    r = 9
  } else if(n_objs >= 5 && n_objs < 10){
    r = 12.5
  } else{
    r = 15
  }
  
  Y1 = rowSums(Y^2) - 16
  Y2 = rowSums(Y^2) - r^2
  
  constraints = Y1*Y2
  constraints = ifelse(constraints < 0, -constraints, 0)
  
  return(constraints)
}

debugSource("../tunning/constraints/C3-DTLZ4.R")

debugSource("../../functions/constraint_dynamic.R")
debugSource("../../functions/constraint_multistaged.R")
debugSource("../../functions/constraint_unfeasible_exploration.R")

scenario                <- irace::defaultScenario()
scenario$seed           <- 123456 # Seed for the experiment
scenario$targetRunner   <- "target.runner" # Runner function (def. below)
scenario$forbiddenFile  <- "forbidden.txt" # forbidden configs
scenario$debugLevel     <- 1
scenario$maxExperiments <- 300 # Tuning budget
scenario$testNbElites   <- 3     # test all final elite configurations

n_variables = 5 ##################################

# Number of cores to be used by irace (set with caution!)
#nc                      <- parallel::detectCores() - 1
#scenario$parallel       <- nc

# Read tunable parameter list from file
parameters <- readParameters("parameters.txt")


################# Build training instances ################# 
fname   <- c("C1-DTLZ1","C1-DTLZ3","C2-DTLZ2","C3-DTLZ1","C3-DTLZ4")
dims    <- c(2,3,5)

allfuns            <- expand.grid(fname, dims, stringsAsFactors = FALSE)
scenario$instances <- paste0(allfuns[,1], "_", allfuns[,2])

for (i in 1:nrow(allfuns)){
  assign(x     = scenario$instances[i],
        value  = make_vectorized_smoof(prob.name  = substr(allfuns[i,1],4,8),
                                       dimensions = n_variables, ################################
                                       n.objectives = allfuns[i,2]))
}

################## Build test instances ##################

dims                   <- c(2)
allfuns                <- expand.grid(fname, dims, stringsAsFactors = FALSE)
scenario$testInstances <- paste0(allfuns[,1], "_", allfuns[,2])

for (i in 1:nrow(allfuns)){
  assign(x     = scenario$testInstances[i],
         value  = make_vectorized_smoof(prob.name  = substr(allfuns[i,1],4,8),
                                        dimensions = n_variables, ################################
                                        n.objectives = allfuns[i,2]))
}

target.runner <- function(experiment, scenario){
  force(experiment)
  conf <- experiment$configuration
  inst <- experiment$instance
  
  # Assemble moead input lists
  ## 1. Problem
  fdef    <- unlist(strsplit(inst, split = "_"))
  problem_name = substr(fdef[1],1,8)

  if(problem_name == "C1-DTLZ1"){
    name = "my_constraints_C1DTLZ1"
  }else if(problem_name == "C1-DTLZ3"){
    name = "my_constraints_C1DTLZ3"
  }else if(problem_name == "C2-DTLZ2"){
    name = "my_constraints_C2DTLZ2"
  }else if(problem_name == "C3-DTLZ1"){
    name = "my_constraints_C3DTLZ1"
  }else if(problem_name == "C3-DTLZ4"){
    name = "my_constraints_C3DTLZ4"
  }
  
  problem <- list(name       = inst,
                  xmin       = rep(0, n_variables),
                  xmax       = rep(1, n_variables),
                  constraints = list(name = name),
                  m          = as.numeric(fdef[2]))

  
  ## 2. Decomp
  decomp <- list(name = "SLD") 
  if(fdef[2] == "2"){
    decomp$H = 99
  }else if(fdef[2] == "3"){
    decomp$H = 14
  }else if(fdef[2] == "5"){
    decomp$H = 6
  }
    
  ## 3. Neighbors
  neighbors <- list(name    = "lambda",
                    delta.p = 0.9) 
  if(fdef[2] == "2"){
    neighbors$T = floor(100*0.2)
  }else if(fdef[2] == "3"){
    neighbors$T = floor(126*0.2)
  }else if(fdef[2] == "5"){
    neighbors$T = floor(105*0.2)
  }
  
  ## 4. Aggfun
  aggfun <- list(name = "wt")
  
  ## 5. Update
  update <- list(name = "standard")
  
  ## 6. Scaling
  scaling <- list(name = "simple")
  
  ## 7. Constraint
  constraint<- list(name = "penalty", beta = conf$beta)
  
  ## 8. Stop criterion
  stopcrit  <- list(list(name    = "maxeval",
                         maxeval = 10000))
  
  ## 9. Echoing
  showpars  <- list(show.iters = "none")
  
  ## 10. Variation stack
  variation <- list(list(name  = "sbx",
                         etax  = 20, pc = 1),
                    list(name  = "polymut",
                         etam  = 20, pm = 1/n_variables),
                    list(name  = "truncate"))
  
  ## 11. Seed
  seed <- conf$seed
  
  # Run MOEA/D
  out <- moead(preset = NULL,
               problem, decomp,  aggfun, neighbors, variation, update,
               constraint, scaling, stopcrit, showpars, seed)
  
  # Hypervolume Calculation
  # No feasible solution
  if(max(out$V$v == 0) == 0){
    HV = 0
  }
  # At least one feasible solution
  else{
    HV = dominated_hypervolume(t(out$Y[which(out$V$v == 0),]), rep(11,as.numeric(fdef[2])))
  }
  
  return(list(cost = HV))
}

## Running the experiment
irace.output <- irace::irace(scenario, parameters)
saveRDS(irace.output, "RESULTS.rds")
file.copy(from = "irace.Rdata", to = "irace-tuning.Rdata")


## Test returned configurations on test instances
testing.main(logFile = "irace-tuning.Rdata")
file.copy(from = "irace.Rdata", to = "irace-testing.Rdata")