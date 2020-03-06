
#' @title sim 
#' @description Simulates outcomes from a model with parameter uncertainties.
#' @param data A dataset
#' @param model Model to simulate with. Must support vcov and coef
#' @param n Number of draws 
#' @param vcovFn function that produces a VCOV 
#' @export
sim <- function(data,model,n = 1000,vcovFn=vcov,interval=c(0.025,0.975)){
   if(!"(Intercept)" %in% names(data)){
      data["(Intercept)"] <- 1
   } 
   missing <- setdiff(names(coef(model)),names(data))
   if(length(missing) > 1){
      stop(paste0("Missing variables:\n",glue::glue_collapse(missing,sep = "\n")))
   }

   betas <- MASS::mvrnorm(n, coef(model), vcovFn(model))
   mat <- as.matrix(data[,names(coef(model))])
   y <- mat %*% t(betas)

   probs <- c(interval[1],.5,interval[2])
   quantiles <- apply(y, 1, quantile, probs = probs,na.rm = T)
   out <- data.frame(t(quantiles))
   names(out) <- c("sim_lower","sim_mean","sim_upper")
   out
}

