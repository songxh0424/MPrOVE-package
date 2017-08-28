#' checkLike
#' 
#' @param str A character string. Used to match the pattern of 
#' the first few digits of an ICD10 code. 
#' @param codes A vector of ICD10 codes as strings. Exclusions
#' @param dict A vector of ICD10 codes as strings.
#' @return Returns \code{TRUE} if all strings in \code{dict} starting with 
#' \code{str} are in \code{codes}, \code{FALSE} otherwise. 
#' @export
checkLike <- function(str,codes,dict){
  # returns TRUE if all strings in dict starting with 'str' are in codes 
  length(setdiff(dict[grep(sprintf('^%s',str),dict)],
                 codes[grep(sprintf('^%s',str),codes)])) == 0
}