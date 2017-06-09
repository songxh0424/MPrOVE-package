#' Convert between ICD9 codes and ICD10 codes
#' 
#' @param ICD9vec A vector of ICD9 codes to convert
#' @param ICD10vec A vector of ICD10 codes to convert
#' @param return.value By default only ICD10 codes are returned, set to 'All' to also 
#' return flags.
#' @param option Choose from 1 to 3 to specify what table to be used in conversion.
#' Choose 1 for ICD9to10, 2 for ICD10to9, and 3 for using both and returning the union of
#' two sets of results. Default for \code{getICD10} is 1, while default for \code{getICD9} is 2.
#' @return If \code{return.value='ICD10only'} a vector of ICD10 codes 
#' matching the ICD9 codes given (duplicates are omitted). Otherwise, if  \code{return.value='All'}
#' a data frame consisting of rows from \code{ICD9to10} matching 
#' the requested ICD9 codes. 
#' @details For \code{getICD10}, the conversion is done using \code{ICD9to10} dataset by default. 
#' For \code{getICD9}, the conversion is done using \code{ICD10to9} dataset by default. If option 
#' is set to 3 and return.value is set to 'All', then an additional column \code{From} is created
#' to indicate the source (ICD9to10 or ICD10to9) of each line of conversion result. 
#' @examples 
#' ICD9vec <- as.character(c(30781,33900:33999,3460:3469))
#' ICD10vec <- getICD10(ICD9vec)
#' head(ICD10vec)
#' @export
getICD10 <- function(ICD9vec,return.value=c('ICD10only','All'),option=c("1","2","3")){
  data("ICD9to10", "ICD10to9")
  rv <- match.arg(return.value,c('ICD10only','All'))
  op <- match.arg(as.character(option),c("1","2","3"))
  if(op == "1") dat <- ICD9to10
  if(op == "2") dat <- ICD10to9
  if(op == "3") {
    dat <- bind_rows(mutate(ICD9to10,From="ICD9to10"),
                     mutate(ICD10to9,From="ICD10to9"))
  } 
  idx <- sapply(ICD9vec, function(ICD9) {
    grep(sprintf('^%s', ICD9), dat$ICD9)
  })
  cd <- dat[unique(unlist(idx)), ]
  rownames(cd) <- NULL
  if(rv=='All') return(cd)
  return(unique(cd$ICD10))
}
#' @rdname getICD10
#' @export
getICD9 <- function(ICD10vec,return.value=c('ICD9only','All'),option=c("2","1","3")) {
  data("ICD9to10", "ICD10to9")
  rv <- match.arg(return.value,c('ICD9only','All'))
  op <- match.arg(as.character(option),c("2","1","3"))
  if(op == "1") dat <- ICD9to10
  if(op == "2") dat <- ICD10to9
  if(op == "3") {
    dat <- bind_rows(mutate(ICD9to10,From="ICD9to10"),
                     mutate(ICD10to9,From="ICD10to9"))
  }
  idx <- sapply(ICD10vec, function(ICD10) {
    grep(sprintf('^%s', ICD10), dat$ICD10)
  })
  cd <- dat[unique(unlist(idx)), ]
  rownames(cd) <- NULL
  if(rv=='All') return(cd)
  return(unique(cd$ICD9))
}
