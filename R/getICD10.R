#' Convert between ICD9 codes and ICD10 codes
#' 
#' @param ICD9vec A vector of ICD9 codes to convert
#' @param ICD10vec A vector of ICD10 codes to convert
#' @param return.value By default only ICD10 codes are returned, set to 'All' 
#' to also return flags
#' @return If \code{return.value='ICD10only'} a vector of ICD10 codes 
#' matching the ICD9 codes given (duplicates are omitted). Otherwise, if  \code{return.value='All'}
#' a data frame consisting of rows from \code{ICD9to10} matching 
#' the requested ICD9 codes. 
#' @details For \code{getICD10}, the conversion is done using \code{ICD9to10} dataset. 
#' For \code{getICD9}, the conversion is done using \code{ICD10to9} dataset. 
#' @examples 
#' ICD9vec <- as.character(c(30781,33900:33999,3460:3469))
#' ICD10vec <- getICD10(ICD9vec)
#' head(ICD10vec)
#' @export
getICD10 <- function(ICD9vec,return.value=c('ICD10only','All')){
  if(!{'ICD9to10' %in% ls(envir=.GlobalEnv)}){
    data(ICD9to10)
  }
  rv <- match.arg(return.value,c('ICD10only','All'))
  idx <- sapply(ICD9vec, function(ICD9) {
    grep(sprintf('^%s', ICD9), ICD9to10$ICD9)
  })
  cd <- ICD9to10[unique(unlist(idx)), ]
  if(rv=='All') return(cd)
  return(unique(cd$ICD10))
}
#' @rdname getICD10
#' @export
getICD9 <- function(ICD10vec, return.value = c('ICD9only', 'All')) {
  if(!{'ICD10to9' %in% ls(envir=.GlobalEnv)}){
    data(ICD10to9)
  }
  rv <- match.arg(return.value,c('ICD9only','All'))
  idx <- sapply(ICD10vec, function(ICD10) {
    grep(sprintf('^%s', ICD10), ICD10to9$ICD10)
  })
  cd <- ICD10to9[unique(unlist(idx)), ]
  if(rv=='All') return(cd)
  return(unique(cd$ICD9))
}