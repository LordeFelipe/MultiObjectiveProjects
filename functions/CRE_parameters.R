#' Returns all the important parameters of a specific RE problem
#'
#' This routine return important information used to run the MOEA/D algorithm
#' like the maximum and minimum of each variable, the number of variables, 
#' number of constraints and number of objectives, 
#' 
#' @param problem String containing the name of the problem in the format CREXX
#'
#' @return A list containing all the problem's important variables

CRE_parameters <- function(problem){
  if(problem == "CRE21"){
    return(list(n_variables = 3,
                n_objectives = 2,
                n_constraints = 3,
                minimum = c(0.0001, 0.0001, 1.0),
                maximum = c(100.0, 100.0, 3.0)))
  }
  else if(problem == "CRE22"){
    return(list(n_variables = 4,
                n_objectives = 2,
                n_constraints = 4,
                minimum = c(0.125, 0.1, 0.1, 0.125),
                maximum = c(5.0, 10.0, 10.0, 5.0)))
  }
  else if(problem == "CRE23"){
    return(list(n_variables = 4,
                n_objectives = 2,
                n_constraints = 4,
                minimum = c(55, 75, 1000, 11),
                maximum = c(80, 110, 3000, 20)))
  }
  else if(problem == "CRE31"){
    return(list(n_variables = 7,
                n_objectives = 3,
                n_constraints = 10,
                minimum = c(0.5,0.45,0.5,0.5,0.875,0.4,0.4),
                maximum = c(1.5,1.35,1.5,1.5,2.625,1.2,1.2)))
  }
  else if(problem == "CRE32"){
    return(list(n_variables = 6,
                n_objectives = 3,
                n_constraints = 9,
                minimum = c(150,20,13,10,14,0.63),
                maximum = c(274.32,32.31,25.0,11.71,18.0,0.75)))
  }
  else if(problem == "CRE51"){
    return(list(n_variables = 3,
                n_objectives = 5,
                n_constraints = 7,
                minimum = c(0.01,0.01,0.01),
                maximum = c(0.45,0.1,0.1)))
  }
  else{
    cat("This problem does not exist")
  }
}