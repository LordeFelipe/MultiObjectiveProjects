#Objective1
Objective1 <- function(X){ 
  
  x_L = X[1]
  x_B = X[2]
  x_D = X[3]
  x_T = X[4]
  x_Vk = X[5]
  x_CB = X[6]
  
  displacement = 1.025 * x_L * x_B * x_T * x_CB;
  V = 0.5144 * x_Vk;
  pg = 9.8065;
  Fn = V /(pg * x_L)^0.5
  a = (4977.06 * x_CB * x_CB) - (8105.61 * x_CB) + 4456.51
  b = (-10847.2 * x_CB * x_CB) + (12817.0 * x_CB) - 6960.32
  
  power = (displacement)^(2.0/3.0) * (x_Vk)^3 / (a + (b * Fn))
  outfit_weight = 1.0 * (x_L^0.8) * (x_B^0.6) * (x_D^0.3) * (x_CB^0.1)
  steel_weight = 0.034 * (x_L^1.7) * (x_B^0.7) * (x_D^0.4) * (x_CB^0.5)
  machinery_weight = 0.17 * (power^0.9)
  light_ship_weight = steel_weight + outfit_weight + machinery_weight;
  
  ship_cost = 1.3 * ((2000.0 * (steel_weight^0.85))  + (3500.0 * outfit_weight) + (2400.0 * (power^0.8)))
  capital_costs = 0.2 * ship_cost
  
  DWT = displacement - light_ship_weight
  
  running_costs = 40000.0 * (DWT^0.3)
  
  round_trip_miles = 5000.0;
  sea_days = (round_trip_miles / 24.0) * x_Vk;
  handling_rate = 8000.0
  
  daily_consumption = ((0.19 * power * 24.0) / 1000.0) + 0.2;
  fuel_price = 100.0;
  fuel_cost = 1.05 * daily_consumption * sea_days * fuel_price;
  port_cost = 6.3 * (DWT^0.8);
  
  fuel_carried = daily_consumption * (sea_days + 5.0)
  miscellaneous_DWT = 2.0 * (DWT^0.5)
  
  cargo_DWT = DWT - fuel_carried - miscellaneous_DWT
  port_days = 2.0 * ((cargo_DWT / handling_rate) + 0.5)
  RTPA = 350.0 / (sea_days + port_days)
  
  voyage_costs = (fuel_cost + port_cost) * RTPA
  annual_costs = capital_costs + running_costs + voyage_costs
  annual_cargo = cargo_DWT * RTPA
  
  
  obj1 = matrix(annual_costs / annual_cargo, ncol = 1)
  obj1
}

#Objective2
Objective2 <- function(X){
  
  x_L = X[1]
  x_B = X[2]
  x_D = X[3]
  x_T = X[4]
  x_Vk = X[5]
  x_CB = X[6]
  
  displacement = 1.025 * x_L * x_B * x_T * x_CB;
  V = 0.5144 * x_Vk;
  pg = 9.8065;
  Fn = V /(pg * x_L)^0.5
  a = (4977.06 * x_CB * x_CB) - (8105.61 * x_CB) + 4456.51
  b = (-10847.2 * x_CB * x_CB) + (12817.0 * x_CB) - 6960.32
  
  power = (displacement)^(2.0/3.0) * (x_Vk)^3 / (a + (b * Fn))
  outfit_weight = 1.0 * (x_L^0.8) * (x_B^0.6) * (x_D^0.3) * (x_CB^0.1)
  steel_weight = 0.034 * (x_L^1.7) * (x_B^0.7) * (x_D^0.4) * (x_CB^0.5)
  machinery_weight = 0.17 * (power^0.9)
  light_ship_weight = steel_weight + outfit_weight + machinery_weight;
  
  ship_cost = 1.3 * ((2000.0 * (steel_weight^0.85))  + (3500.0 * outfit_weight) + (2400.0 * (power^0.8)))
  capital_costs = 0.2 * ship_cost
  
  DWT = displacement - light_ship_weight
  
  running_costs = 40000.0 * (DWT^0.3)
  
  round_trip_miles = 5000.0;
  sea_days = (round_trip_miles / 24.0) * x_Vk;
  handling_rate = 8000.0
  
  daily_consumption = ((0.19 * power * 24.0) / 1000.0) + 0.2;
  fuel_price = 100.0;
  fuel_cost = 1.05 * daily_consumption * sea_days * fuel_price;
  port_cost = 6.3 * (DWT^0.8);
  
  fuel_carried = daily_consumption * (sea_days + 5.0)
  miscellaneous_DWT = 2.0 * (DWT^0.5)
  
  cargo_DWT = DWT - fuel_carried - miscellaneous_DWT
  port_days = 2.0 * ((cargo_DWT / handling_rate) + 0.5)
  RTPA = 350.0 / (sea_days + port_days)
  
  voyage_costs = (fuel_cost + port_cost) * RTPA
  annual_costs = capital_costs + running_costs + voyage_costs
  annual_cargo = cargo_DWT * RTPA
  
  obj2 = matrix(light_ship_weight, ncol = 1)
  obj2
}

#Objective3
Objective3 <- function(X){
  
  x_L = X[1]
  x_B = X[2]
  x_D = X[3]
  x_T = X[4]
  x_Vk = X[5]
  x_CB = X[6]
  
  displacement = 1.025 * x_L * x_B * x_T * x_CB;
  V = 0.5144 * x_Vk;
  pg = 9.8065;
  Fn = V /(pg * x_L)^0.5
  a = (4977.06 * x_CB * x_CB) - (8105.61 * x_CB) + 4456.51
  b = (-10847.2 * x_CB * x_CB) + (12817.0 * x_CB) - 6960.32
  
  power = (displacement)^(2.0/3.0) * (x_Vk)^3 / (a + (b * Fn))
  outfit_weight = 1.0 * (x_L^0.8) * (x_B^0.6) * (x_D^0.3) * (x_CB^0.1)
  steel_weight = 0.034 * (x_L^1.7) * (x_B^0.7) * (x_D^0.4) * (x_CB^0.5)
  machinery_weight = 0.17 * (power^0.9)
  light_ship_weight = steel_weight + outfit_weight + machinery_weight;
  
  ship_cost = 1.3 * ((2000.0 * (steel_weight^0.85))  + (3500.0 * outfit_weight) + (2400.0 * (power^0.8)))
  capital_costs = 0.2 * ship_cost
  
  DWT = displacement - light_ship_weight
  
  running_costs = 40000.0 * (DWT^0.3)
  
  round_trip_miles = 5000.0;
  sea_days = (round_trip_miles / 24.0) * x_Vk;
  handling_rate = 8000.0
  
  daily_consumption = ((0.19 * power * 24.0) / 1000.0) + 0.2;
  fuel_price = 100.0;
  fuel_cost = 1.05 * daily_consumption * sea_days * fuel_price;
  port_cost = 6.3 * (DWT^0.8);
  
  fuel_carried = daily_consumption * (sea_days + 5.0)
  miscellaneous_DWT = 2.0 * (DWT^0.5)
  
  cargo_DWT = DWT - fuel_carried - miscellaneous_DWT
  port_days = 2.0 * ((cargo_DWT / handling_rate) + 0.5)
  RTPA = 350.0 / (sea_days + port_days)
  
  voyage_costs = (fuel_cost + port_cost) * RTPA
  annual_costs = capital_costs + running_costs + voyage_costs
  annual_cargo = cargo_DWT * RTPA  
  obj3 = matrix(-annual_cargo, ncol = 1)
  obj3
}

#Definition of the problem
problem.cre32 <- function(X) { ###
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(Objective1(X), Objective2(X), Objective3(X)) } ###
  ))
}

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
                         paste0("g",
                                rep(1:n_constraints, times = 1)))
  
  # Box limits of the feasible space
  Xmin <- matrix(minimum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(maximum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  g1 <- function(X){
    write(t(X),file = paste(getwd(), "CREProblems/CRE32/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = " ")
    system("CREProblems/CRE32/example", ignore.stdout = TRUE)
    constraints <- scan(paste(getwd(), "CREProblems/CRE32/pop_vars_cons.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = n_constraints, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  
  # Inequality constraints
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)  
  
  v = rowSums(Vmatrix)  
  # Before the first generation when there is no incubent solutions to scale yet
  if(is.null(parent.frame(2)$iter)){
    v[which(v != 0)] = (v[which(v != 0)] - min(v))/(max(v) - min(v)) + 0.000001
  }
  # Case of all other generations
  else{
    e = parent.frame(2)
    Vtmatrix = e$Vt$Vmatrix
    vt = rowSums(Vtmatrix)
    e$Vt$v[which(vt != 0)] = (vt[which(vt != 0)] - min(v,vt))/(max(v,vt) - min(v,vt)) + 0.000001
    v[which(v != 0)] = (v[which(v != 0)] - min(v,vt))/(max(v,vt) - min(v,vt)) + 0.000001
  }
  
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}