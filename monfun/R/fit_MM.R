fit_MM <- function(data, behavior_list, par = matrix(1/length(behavior_list), 
                                                  nrow = length(behavior_list), 
                                                  ncol = length(behavior_list) - 1)){
  fit <- optim(par = par, fn = function(par, data){
    if(any(par < 0)) return(-Inf)
    if(any(par > 1)) return(-Inf)
    Pf <- matrix(par, nrow = length(behavior_list))
    if(any(rowSums(Pf) > 1)) return(-Inf)
    Pf <- cbind(Pf, 1 - rowSums(Pf))
    rownames(Pf) <- colnames(Pf) <- behavior_list
    get_ll(X, Pf, behavior_list)}, data = data, control = c(fnscale = -1))
  
  out <- fit$par
  out <- cbind(out, 1 - rowSums(out))
  rownames(out) <- colnames(out) <- behavior_list
  return(out)
}