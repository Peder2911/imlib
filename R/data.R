
#' @title formatDims 
#' @description Create a printable representation of a dim vector.
#' @param dims A vector

formatDims <- function(dims){
   paste(dims,sep=" x ")
}

#' @title makeTestSet
#' @description Create a test set from a dataset. The resulting dataset
#' contains every permutational combination of variable values from the
#' variables list. This dataset is useful for testing counterfactuals.
#' @param data A dataset
#' @param variables A list of variables. The names should correspond to the
#' names of variables in the dataset, while the values should be the values you
#' want the variables to have. 
#' @param avgfun Function to produce placeholder values for variables not
#' included in the variables list.
#' @export
makeTestSet <- function(data,variables,avgfun){
   if(!all(names(variables) %in% names(data))){
      missing <- setdiff(names(variables),names(data))
      stop(paste("Missing variables:",glue::glue_collapse(missing,sep = "\n"),sep = "\n"))
   }

   tgtShape <- list(
      rows = purrr::reduce(sapply(variables,length), `*`),
      columns = ncol(data)
   )

   out <- as.data.frame(expand.grid(variables))

   constants <- data[!names(data) %in% names(variables)]
   constants <- lapply(constants,avgfun,na.rm=T)

   for(v in names(constants)){
      out[v] <- constants[[v]] 
   }


   if(!all(nrow(out) == tgtShape$rows, ncol(out) == tgtShape$columns)){
      stop(paste("Wrong number of dimensions:", formatDims(dim(out)),formatDims(dim(tgtShape))))
   }

   out
}

