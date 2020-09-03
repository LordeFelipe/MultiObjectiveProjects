suppressPackageStartupMessages(library(irace))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(emoa))
suppressPackageStartupMessages(library(smoof))
suppressPackageStartupMessages(library(MOEADr))
suppressPackageStartupMessages(library(combinat))
library(tictoc)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

tic()

scenario                <- irace::defaultScenario()
scenario$seed           <- 123456 # Seed for the experiment
scenario$targetRunner   <- "target.runner" # Runner function (def. below)
scenario$forbiddenFile  <- "forbidden.txt" # forbidden configs
scenario$debugLevel     <- 1
scenario$maxExperiments <- 300 # Tuning budget
scenario$testNbElites   <- 7     # test all final elite configurations

n_variables = 5 ##################################

# Read tunable parameter list from file
parameters <- readParameters("parameters.txt")


################# Build training instances ################# 
fname   <- c(paste0("DTLZ",c(1:6)))
dims    <- c(2,3,5)
nvar    <- c(222,5,32)

allfuns            <- expand.grid(fname, dims, nvar,stringsAsFactors = FALSE)
scenario$instances <- paste0(allfuns[,1], "_", allfuns[,2], "_", allfuns[,3])

for (i in 1:nrow(allfuns)){
  assign(x     = scenario$instances[i],
        value  = make_vectorized_smoof(prob.name  = allfuns[i,1],
                                       dimensions = allfuns[i,3],
                                       n.objectives = allfuns[i,2]))
}

################## Build test instances ##################

dims                   <- c(2)
nvar                   <- c(222)
allfuns                <- expand.grid(fname, dims, nvar,stringsAsFactors = FALSE)
scenario$testInstances <- paste0(allfuns[,1], "_", allfuns[,2])

for (i in 1:nrow(allfuns)){
  assign(x     = scenario$testInstances[i],
         value  = make_vectorized_smoof(prob.name  = allfuns[i,1],
                                        dimensions = allfuns[i,3],
                                        n.objectives = allfuns[i,2]))
}

target.runner <- function(experiment, scenario){
  force(experiment)
  conf <- experiment$configuration
  inst <- experiment$instance
  
  # Assemble moead input lists
  ## 1. Problem
  fdef    <- unlist(strsplit(inst, split = "_"))
  problem_name = fdef[1]
  
  problem <- list(name       = inst,
                  xmin       = rep(0, fdef[3]),
                  xmax       = rep(1, fdef[3]),
                  m          = as.numeric(fdef[2]))

  
  ## 2. Decomp
  decomp <- list(name = "SLD") 
  if(fdef[2] == "2"){
    decomp$H = 99
  }else if(fdef[2] == "3"){
    decomp$H = 14
  }else if(fdef[2] == "5"){
    decomp$H = 8
  }
    
  ## 3. Neighbors
  neighbors <- list(name    = "lambda",
                    delta.p = conf$delta.p) 
  if(fdef[2] == "2"){
    neighbors$T = round(99*conf$T.p)
  }else if(fdef[2] == "3"){
    neighbors$T = round(105*conf$T.p)
  }else if(fdef[2] == "5"){
    neighbors$T = round(120*conf$T.p)
  }
  
  ## 4. Aggfun
  aggfun <- list(name = "wt")
  
  ## 5. Update
  update <- list(name = "standard")
  
  ## 6. Scaling
  scaling <- list(name = "simple")
  
  ## 7. Constraint
  constraint<- list(name = "none")
  
  ## 8. Stop criterion
  stopcrit  <- list(list(name    = "maxeval",
                         maxeval = 10000))
  
  ## 9. Echoing
  showpars  <- list(show.iters = "none")
  
  ## 10. Variation stack
  variation <- list(list(name  = "sbx",
                         etax  = conf$sbx.eta, pc = conf$sbx.pc),
                    list(name  = "polymut",
                         etam  = conf$polymut.eta, pm = conf$polymut.pm),
                    list(name  = "truncate"))
  
  ## 11. Seed
  seed <- conf$seed
  
  # Run MOEA/D
  out <- moead(preset = NULL,
               problem, decomp,  aggfun, neighbors, variation, update,
               constraint, scaling, stopcrit, showpars, seed)
  
  # Hypervolume Calculation
  HV = dominated_hypervolume(t(out$Y), rep(11,as.numeric(fdef[2])))
  
  return(list(cost = HV))
}

## Running the experiment
irace.output <- irace::irace(scenario, parameters)
saveRDS(irace.output, "RESULTS.rds")
file.copy(from = "irace.Rdata", to = "irace-tuning.Rdata")


toc()

## Test returned configurations on test instances
#testing.main(logFile = "irace-tuning.Rdata")
#file.copy(from = "irace.Rdata", to = "irace-testing.Rdata")