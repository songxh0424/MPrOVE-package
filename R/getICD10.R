#' Convert ICD9 codes to ICD10 codes
#' 
#' @param ICD9vec A vector of ICD9 codes to convert
#' @param return.value By default only ICD10 codes are returned, set to 'All' 
#' to also return flags
#' @return If \code{return.value='ICD10only'} a vector of ICD10 codes 
#' matching the ICD9 codes given. Otherwise, if  \code{return.value='All'}
#' a matrix consisting of rows from \code{ICD9to10} matching 
#' the requested ICD9 codes. 
#' @export
getICD10 <- function(ICD9vec,return.value=c('ICD10only','All')){
  if(!{'ICD9to10' %in% ls()}){
    data(ICD9to10)
  }
  rv <- match.arg(return.value,c('ICD10only','All'))

  cd <-ICD9to10[unique(unlist(
    sapply(ICD9vec,function(ICD9) grep(ICD9,ICD9to10[,1])))),]
  if(rv=='All') return(cd)
  return(cd[,2])
}