#' Add description to ICD-9 codes
#' @description Given a string of codes and description mixed together, clean up the string to get
#' a dataframe of codes matched with description
#' @param str A character string containing ICD-9 codes and descriptions 
#' in parentheses. Copied from supplement eTable1.
#' @return A data frame, with two columns:
#' \item{ICD9}{The ICD-9 codes that are extracted from \code{str}. }
#' \item{Description}{Description in eTable1 matching the ICD-9 code. }
#' @examples 
#' str <- "7802 9921 (syncope), 345xx 7803x (epilepsy or convulsions), 43xx (cerebrovascular diseases, including stroke/TIA and subarachnoid hemorrhage), 800xx-804xx 850xx-854xx 870xx-873xx 9590x 910xx 920xx-921xx (head or face trauma), 78097 781xx 7820 7845x (altered mental status, nervous and musculoskeletal system symptoms, including gait abnormality, meningismus, disturbed skin sensation, speech deficits), V1254 V10xx (personal history of stroke/TIA )"
#' View(getDesc(str))
#' @export
getDesc <- function(str) {
  ## given a string of codes and description, get a vector of codes and a dataframe of
  ## codes matched with description
  
  leftpar <- str_locate_all(str, "\\(")[[1]][, 1]
  rightpar <- str_locate_all(str, "\\)")[[1]][, 1]
  # extract descriptions
  strInPar <- str_sub(str, start = leftpar, end = rightpar)
  
  # remove descriptions and get codes
  codes <- str_replace_all(str, "\\(.*?\\)", "~")
  codes <- str_replace_all(codes, ",", "")
  strInPar <- str_replace_all(strInPar, "[\\(\\)]", "")
  
  # split codes into groups
  groups <- str_trim(str_split(codes, "~")[[1]])
  if(length(strInPar) > 0) groups <- groups[-length(groups)]

  groups <- str_replace_all(groups, " *- *", "-")
  groups <- str_split(groups, " +")
  groups <- lapply(groups, removex)
  leng <- sapply(groups, length)
  description <- rep(strInPar, leng)
  codes <- unlist(groups)
  if(length(strInPar) == 0) {
    warning("No descriptions found!")
    return(data.frame(ICD9 = codes, stringsAsFactors = F))
  }
  return(data.frame(ICD9 = codes, Description = description, 
                    stringsAsFactors = FALSE))
}
