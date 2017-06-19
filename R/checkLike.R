#' checkLike
#' 
#' @param str A character string. Used to match the pattern of 
#' the first few digits of an ICD10 code. 
#' @param codes A vector of ICD10 codes as strings. Exclusions
#' @param dict A vector of ICD10 codes as strings.
#' @param thres A percentage threshold for passing the check. 
#' @return Returns \code{TRUE} if all strings in \code{dict} starting with 
#' \code{str} are in \code{codes}, \code{FALSE} otherwise. 
#' @export
checkLike <- function(str,codes,dict,thres=100){
  # returns TRUE if all strings in dict starting with 'str' are in codes 
  tmp = length(setdiff(dict[grep(sprintf('^%s',str),dict)],
                       codes[grep(sprintf('^%s',str),codes)])) / 
        length(dict[grep(sprintf('^%s',str),dict)]) <= 1 - thres/100
  if(is.na(tmp)) return(FALSE)
  else return(tmp)
}
