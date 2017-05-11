#' Add periods for ICD codes
#' @description This function add a period after the third digit of an ICD 
#' code and return the result as a string. 
#' @param x A character vector that represents a series of ICD codes.
#' @return A character vector of the same length as \code{x}. Each ICD code in 
#' the vector has a period added after the third digit. 
#' @examples 
#' bpEligICD9 <- c(7213,72190,72210,72252,7226,72293,72402,7242:7246,72470,72471, 
#'                 72479,7385,7393,7394,8460:8469)
#' bpElibICD10 <- unique(getICD10(bpEligICD9))
#' addPeriods(bpElibICD10)
addPeriods <- function(x){
  paste(stringr::str_sub(x, 1, 3), '.', stringr::str_sub(x, start = 4), sep = '')
}