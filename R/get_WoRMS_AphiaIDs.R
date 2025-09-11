#' @name get_WoRMS_AphiaID
#' @title get_WoRMS_AphiaID
#' @description Tries to get a WoRMS Aphia ID from a WoRMS using taxa matching. 
#' The function is wrapper on top of \link[worrms]{wm_records_taxamatch} function.
#' 
#' @param name a species scientific name
#' @return the corresponding AphiaID if any, \code{NA} otherwise
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
get_WoRMS_AphiaID <- function(name){
  aphiaID = NA
  raw_res = try(worrms::wm_records_taxamatch(name = name, marine_only = FALSE, useragent = paste0("fdi4R_", uuid::UUIDgenerate())))
  if(!is(raw_res, "try-error")){
    aphiaID = raw_res[[1]]$AphiaID[1]
  }
  return(aphiaID)
}

#' @name get_WoRMS_AphiaIDs
#' @title get_WoRMS_AphiaIDs
#' @description Tries to get WoRMS Aphia IDs for a series of scientific names. The function
#' supports \code{parallel} mode for fast taxa matching.
#' 
#' @param x a vector of species scientific names
#' @return a vector of the corresponding AphiaIDs if any, \code{NA} otherwise
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
get_WoRMS_AphiaIDs <- function(x, parallel = FALSE, parallel_handler = NULL, cl = NULL, ...){
  download_results = list()
  
  #files download
  if(parallel){
    if (is.null(parallel_handler)) {
      errMsg <- "No 'parallel_handler' specified"
      stop(errMsg)
    }
    if(!is.null(parallel_handler)){
      if(!is.null(cl)){
        if (!requireNamespace("parallel", quietly = TRUE)) {
          errMsg <- "Package \"parallel\" needed for cluster-based parallel handler. Please install it."
          stop(errMsg)
        }
        download_results <- invisible(parallel_handler(cl, x, get_WoRMS_AphiaID, ...))
        try(parallel::stopCluster(cl))
      }else{
        download_results <- invisible(parallel_handler(x, get_WoRMS_AphiaID, ...))
      }
    }
  }else{
    download_results <- invisible(lapply(x, get_WoRMS_AphiaID))
  }
  return(download_results)
}