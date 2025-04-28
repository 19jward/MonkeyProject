fit_MM <- function(data, behavior_list, par = matrix(1/length(behavior_list), 
                                                     nrow = length(behavior_list), 
                                                     ncol = length(behavior_list) - 1), 
                   maxit = 5000, rowwise = T){
  if(!isTRUE(rowwise)){
    fit <- optim(par = par, fn = function(par, data){
      if(any(par < 0)) return(-Inf)
      if(any(par > 1)) return(-Inf)
      Pf <- matrix(par, nrow = length(behavior_list))
      if(any(rowSums(Pf) > 1)) return(-Inf)
      Pf <- cbind(Pf, 1 - rowSums(Pf))
      rownames(Pf) <- colnames(Pf) <- behavior_list
      get_ll(X, Pf, behavior_list)}, data = data, control = c(fnscale = -1, maxit = maxit))
    
    out <- fit$par
    out <- cbind(out, 1 - rowSums(out))
    rownames(out) <- colnames(out) <- behavior_list
    return(out)
    
  } else if(isTRUE(rowwise)){
    out <- matrix(NA, nrow = length(behavior_list), ncol = length(behavior_list))
    rownames(out) <- colnames(out) <- behavior_list
    rownames(par) <- behavior_list
    for(b1 in behavior_list){
      par_row <- par[b1, ]
      fit <- optim(par = par_row, fn = function(par, data){
        if(any(par < 0)) return(-Inf)
        if(any(par > 1)) return(-Inf)
        if(sum(par) > 1) return(-Inf)
        p_row <- c(par, 1 - sum(par))
        names(p_row) <- behavior_list
        get_ll_row(X, p_row, row_behavior = b1, behavior_list)}, data = data,  
        control = c(fnscale = -1, maxit = maxit))
      
      row_out <- fit$par
      row_out <- c(row_out, 1 - sum(row_out))
      
      out[b1, ] <- row_out
    }
    
    return(out)
  }
}