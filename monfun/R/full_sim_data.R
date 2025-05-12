## finalize simulated data combining the 2 previous functions (sim_behavior_sequence, collapse_sim_data)
## also adds a column for habitat and focal
## X can now be multiple rows for a FxM
full_sim_data <- function(X, beta, T_max, num_focals){
  ## do i need to make sure X is a matrix?
  output <- c()
  for (i in 1:num_focals){
    x_i <- X[i, ] ## pull out correct focal from X
    if (is.null(dim(x_i))) {
      x_i <- matrix(x_i, nrow = 1)
    }
    
    ## run the simulate behaviors
    data <- sim_behavior_sequence(x_i, beta, T_max)
    
    ## collapse the simulated behaviors
    data <- collapse_sim_data(data)
    
    ## add FocalID column
    FocalID <- c(rep.int(i, nrow(data)))
    data <- cbind(data, FocalID)
    
    ## add habitat column
    Habitat <- c(rep.int(x_i[1,2], nrow(data)))
    data <- cbind(data, Habitat)
    
    ## put multiple focals together
    output <- rbind(output, data)
    
  } ## end focal for loop
  
  ## replace numbers in habitat with names (NOTE: not 100% sure which is which, need to check)
  output$Habitat <- replace(output$Habitat, output$Habitat == 0, "jalan")
  output$Habitat <- replace(output$Habitat, output$Habitat == 1, "hutan")
  
  return(output)  
} ## end function
