#' Group similar code by their first few characters
#' @description Modified after the source code of \code{convert2regex}. 
#' @return A character vector of the grouping result. 
#' @export
group.I10 <- function(codes, minGroup = 2, maxLength = 4) {
  ## given a vector of I10 codes, group them up as LIKEs or REGEX patterns
  
  data("ICD9to10")
  temp <- convertToLike(codes, ICD9to10$ICD10, minGroup, maxLength)
  likes <- temp$LIKE
  ins <- temp$IN
  nm <- c(names(likes), ins)

  initial <- unique(str_sub(nm, end = 1))
  result <- c()
  for(fst in initial) {
    nms <- nm[grep(sprintf("^%s", fst), nm)]
    leng <- str_length(nms)
    
    result <- append(result, c(nms[leng == 1]))
    if(any(leng == 2)) {
      snd <- str_sub(nms[leng == 2], start = 2, end = 2)
      if(length(snd) == 1) {
        result <- append(result, sprintf("%s%s", fst, snd))
      }
      else {
        result <- append(result, sprintf("%s[%s]", fst, paste(snd, collapse = ",")))
      }
    }
    
    if(any(leng == 3)) {
      second <- str_sub(nms[leng == 3], start = 2, end = 2)
      for(snd in unique(second)) {
        nmss <- nms[leng == 3][grep(sprintf("^%s%s", fst, snd), nms[leng == 3])]
        trd <- str_sub(nmss, start = 3, end = 3)
        if(length(trd) == 1) {
          result <- append(result, sprintf("%s%s%s", fst, snd, trd))
        }
        else {
          result <- append(result, sprintf("%s%s[%s]", fst, snd, 
                                           paste(trd, collapse = ",")))
        }
      }
    }
    
    if(any(leng == 4)) {
      snd2trd <- str_sub(nms[leng == 4], start = 2, end = 3)
      for(s2t in unique(snd2trd)) {
        nmss <- nms[leng == 4][grep(sprintf("^%s%s", fst, s2t), nms[leng ==4])]
        frth <- str_sub(nmss, start = 4, end = 4)
        if(length(frth) == 1) {
          result <- append(result, sprintf("%s%s%s", fst, s2t, frth))
        }
        else {
          result <- append(result, sprintf("%s%s[%s]", fst, s2t, 
                                           paste(frth, collapse = ",")))
        }
      }
    }
  }
  return(result)
}

#' @export
group.ICD <- function(codes, input = c("ICD9", "ICD10", "CPT"), minGroup = 2, maxLength = 4) {
  ## given a vector of I10 codes, group them up as LIKEs or REGEX patterns
  
  data("ICD9to10", "ICD10to9", "dict_CPT")
  ICDvers <- match.arg(input, c("ICD9", "ICD10", "CPT"))
  if(ICDvers == "ICD10") temp <- convertToLike(codes, ICD9to10$ICD10, minGroup, maxLength)
  if(ICDvers == "ICD9") temp <- convertToLike(codes, ICD9to10$ICD9, minGroup, maxLength)
  if(ICDvers == "CPT") temp <- convertToLike(codes, dict_CPT, minGroup, maxLength)
  likes <- temp$LIKE
  ins <- temp$IN
  nm <- c(names(likes), ins)
  
  initial <- unique(str_sub(nm, end = 1))
  result <- c()
  for(fst in initial) {
    nms <- nm[grep(sprintf("^%s", fst), nm)]
    leng <- str_length(nms)
    
    result <- append(result, c(nms[leng == 1]))
    if(any(leng == 2)) {
      snd <- str_sub(nms[leng == 2], start = 2, end = 2)
      if(length(snd) == 1) {
        result <- append(result, sprintf("%s%s", fst, snd))
      }
      else {
        result <- append(result, sprintf("%s[%s]", fst, paste(snd, collapse = ",")))
      }
    }
    
    # lapply(3:7, function(len) {
    #   if(any(leng == len)) {
    #     second <- str_sub(nms[leng == len], start = 2, end = len - 1)
    #     for(snd in unique(second)) {
    #       nmss <- nms[leng == len][grep(sprintf("^%s%s", fst, snd), nms[leng == len])]
    #       trd <- str_sub(nmss, start = len, end = len)
    #       if(length(trd) == 1) {
    #         result <- append(result, sprintf("%s%s%s", fst, snd, trd))
    #       }
    #       else {
    #         result <- append(result, sprintf("%s%s[%s]", fst, snd, 
    #                                          paste(trd, collapse = ",")))
    #       }
    #     }
    #   }
    # })
    for(len in 3:7) {
      if(any(leng == len)) {
        second <- str_sub(nms[leng == len], start = 2, end = len - 1)
        for(snd in unique(second)) {
          nmss <- nms[leng == len][grep(sprintf("^%s%s", fst, snd), nms[leng == len])]
          trd <- str_sub(nmss, start = len, end = len)
          if(length(trd) == 1) {
            result <- append(result, sprintf("%s%s%s", fst, snd, trd))
          }
          else {
            result <- append(result, sprintf("%s%s[%s]", fst, snd, 
                                             paste(trd, collapse = ",")))
          }
        }
      }
    }
    
  }
  return(result)
}
