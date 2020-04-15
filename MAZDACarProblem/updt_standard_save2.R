updt_standard_save2 <- function(X, Xt, Y, Yt, V, Vt, sel.indx, B, ...){
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
  
  #Yaux constains the value of the objective functions in the first m columms and a bolean for the violations in the last one
  #Yaux = matrix(c(ifelse(V$v>0,max(Y[,1]),Y[,1]), ifelse(V$v>0,max(Y[,2]),Y[,2]), ifelse(Vnext$v>0,0,1)), nrow = n_individuals, ncol = 3)
  Yaux = matrix(c(Y[,1], Y[,2], ifelse(Vnext$v>0,0,1)), nrow = n_individuals, ncol = 3)
  write(t(Yaux),file = paste(getwd(), sprintf("MyArchive%d.txt",i), sep="/"), ncolumns = 3, sep = " ", append = TRUE)
  
  # Output
  return(list(X = Xnext,
              Y = Ynext,
              V = Vnext))
}