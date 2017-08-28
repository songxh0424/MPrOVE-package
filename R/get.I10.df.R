#' Create a table of descriptions matched with groups of ICD-10 codes
#' 
#' @param str A character string containing ICD-9 codes and descriptions 
#' in parentheses. Copied from supplement eTable1.
#' @param minGroup The minimum number of occurrences for a starting string pattern
#' to be put in a group. 
#' @param maxLength The maximum length of starting strings considered in \code{$LIKE}.
#' @return A data frame with two colunms: 
#' \item{Description}{Description for the codes, extracted from eTable1. }
#' \item{ICD10}{Groups of ICD-10 codes, represented by the shared starting substring}
#' @export
get.I10.df <- function(str, minGroup = 2, maxLength = 4) {
  ## given an output from getDesc, get the data frame of I10 codes with description
  df <- getDesc(str)
  if(ncol(df) == 1) stop("No description found in str!")
  
  I10 <- lapply(1:nrow(df), function(i) {
    codes <- getICD10(df$ICD9[i])
    description <- rep(df$Description[i], length(codes))
    data.frame(ICD10 = codes, Description = description, 
               stringsAsFactors = FALSE)
  })
  I10 <- bind_rows(I10)
  
  result <- lapply(unique(I10$Description), function(des) {
    codes <- I10$ICD10[I10$Description == des]
    codes <- group.I10(codes, minGroup, maxLength)
    data.frame(Description = rep(des, length(codes)), ICD10 = codes, 
               stringsAsFactors = FALSE)
  })
  
  return(bind_rows(result))
}

#' @export
get.ICD.df <- function(toml, output = c("ICD9", "ICD10"), minGroup = 2, maxLength = 4) {
  ## given a list from parseTOML, get the data frame of ICD codes with description
  ICDvers <- match.arg(output, c("ICD9", "ICD10"))
  lst <- c(toml$denominator$eligible, toml$denominator$exclude)
  description <- str_replace_all(str_replace_all(names(lst), "__", ", "), "_", " ")
  codes <- lapply(lst, function(x) {
    temp <- removex(x$ICD9)
    if(ICDvers == "ICD9") {
      temp <- unique(unlist(lapply(temp, function(str) {
        ICD9to10$ICD9[grep(sprintf("^%s", str), ICD9to10$ICD9)]
      })))
    }
    if(ICDvers == "ICD10") temp <- getICD10(temp)
    return(temp)
  })
  leng <- sapply(codes, length)
  description <- rep(description, leng)
  codes <- unlist(codes)
  df <- data.frame(Description = description, "ICD" = codes, stringsAsFactors = FALSE)
  
  # I10 <- lapply(1:nrow(df), function(i) {
  #   codes <- getICD10(df$ICD9[i])
  #   description <- rep(df$Description[i], length(codes))
  #   data.frame(ICD10 = codes, Description = description, 
  #              stringsAsFactors = FALSE)
  # })
  # I10 <- bind_rows(I10)
  
  result <- lapply(unique(df$Description), function(des) {
    codes <- df$ICD[df$Description == des]
    codes <- group.ICD(codes, input = ICDvers, minGroup, maxLength)
    data.frame(Description = rep(des, length(codes)), ICD = codes, 
               stringsAsFactors = FALSE)
  })
  
  return(bind_rows(result))
}
