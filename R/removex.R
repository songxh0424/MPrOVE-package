#' Remove the x from an ICD-9 code string
#' 
#' @description Given a character string that represents an ICD-9 code or a range
#' of codes, \code{removex()} removes the x in the string and returns a code or a
#' code range that be used directly as input of \code{getICD10()}. 
#' @param string A vector of character strings, may or may not contain characters 
#' 'x' and '-'. 
#' @return A vector of character strings. Each string only contain numeric 
#' characters, and is either a complete ICD-9 code or the first few digits of 
#' an ICD-9 code that represents all codes starting with the same substring.
#' @examples 
#' removex(c("7213", "7213x", "14xx-208xx"))
#' removex("015xx-12xxx")
#' @export
removex <- function(string) {
  ## given a string, remove the x and return the corresponding range
  
  result <- lapply(string, function(str) {
    if(!str_detect(str, "-") & !str_detect(str, "x")) {
      return(str)
    } 
    else if(!str_detect(str, "-")) {
      return(str_replace_all(str, "x", ""))
    }
    else {
      range <- str_trim(str_split(str, "-")[[1]])
      lb <- str_replace_all(range[1], "x", "")
      ub <- str_replace_all(range[2], "x", "")
      # adjust length difference
      leng <- str_length(c(lb, ub))
      lb <- ifelse(leng[1] < leng[2], 
                   paste(c(lb, rep(0, leng[2] - leng[1])), collapse = ""), lb)
      ub <- ifelse(leng[2] < leng[1], 
                   paste(c(ub, rep(9, leng[1] - leng[2])), collapse = ""), ub)
      result <- lb:ub
      # add back leading zeros if necessary
      ori_leng <- str_length(ub)
      leng <- str_length(result)
      result <- sapply(1:length(result), function(i) {
        paste(c(rep(0, ori_leng - leng[i]), result[i]), collapse = "")
      })
      return(result)
    }
  })
  return(unlist(result))
}