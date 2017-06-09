#' Create "REGEXP_LIKE" SQL commands
#' @description This function creates a series of "REGEXP_LIKE" SQL commands, given 
#' a set of starting strings of ICD codes and a starting pattern to match. 
#' @param nm A character vector of starting strings of ICD10 codes. 
#' @param str A specific starting string that is used to match the ones in \code{nm}.
#' Only the matching ones in \code{nm} are considered to create the output SQL 
#' commands. 
#' @param colName The corresponding column name to put in "REGEXP_LIKE" commands.
#' @param add.periods A logical value controlling whether or not to add periods after
#' the third character of ICD codes. 
#' @details \code{str} is usually a very short string. The function first extracts 
#' all the strings in \code{nm} that start with \code{str}. Then those strings are
#' divided into groups that share the first 1, 2, or 3 letters, with only the last
#' letter being different. For each of these groups, a SQL command like this is 
#' created, "OR REGEXP_LIKE (DX_CD,'^I0[1,2,5,6,7,8]','im')".  
#' @return A character string of a series of "REGEXP_LIKE" SQL commands. 
#' @seealso \code{\link{writeSQL}}
#' @examples 
#' bpExclICD9 <- c(1400:2089,2300:2399,8000:8399,8500:8549,8600:8699,
#'                 9050:9099,92611,92612,929,9520:9529,9580:9599,3040:3042,
#'                 3044,3054:3057,34460,7292,4210,4211,4219,0380:0389,01,
#'                 7300:7309,7832,78079,7808,2859)
#' bpExclICD10 <- getICD10(bpExclICD9)
#' codeList <- convertToLike(codes = bpExclICD10, dict = ICD9to10$ICD10,2,4)
#' nm <- names(codeList$LIKE)
#' 
#' cat(convert2regex(nm,'C',colName = 'DX_CD'))
#' @export
convert2regex <- function(nm,str,colName,add.periods=FALSE){
  toBeConverted <- nm[grep(sprintf('^%s',str),nm)]
  # lngth <- sapply(toBeConverted,function(x) length(strsplit(x,'')[[1]]))
  lngth <- stringr::str_length(toBeConverted)
  
  outStr <- '' #iniatilize
  if(any(lngth==2)){
    # second <- sapply(toBeConverted[which(lngth==2)],function(x) strsplit(x,'')[[1]][2])
    second <- stringr::str_sub(toBeConverted[lngth==2], start = 2, end = 2)
    outStr <- sprintf("%sOR REGEXP_LIKE (%s,'^%s[%s]','im')\n",outStr,colName,str,
                      paste(second,collapse=','))
  }
  
  if(any(lngth==3)){
    # second <- sapply(toBeConverted[which(lngth==3)],function(x) strsplit(x,'')[[1]][2])
    second <- stringr::str_sub(toBeConverted[lngth==3], start = 2, end = 2)
    for(sec in unique(second)){
      tbc <- toBeConverted[lngth==3][
        grep(sprintf('^%s',paste(str,sec,sep='')),
             toBeConverted[lngth==3])
        ]
      third <- stringr::str_sub(tbc, start = 3, end = 3) 
      outStr <- sprintf("%sOR REGEXP_LIKE (%s,'^%s%s[%s]','im')\n",
                        outStr,colName,str,sec,
                        paste(third,collapse=','))
    }
  }
  
  if(any(lngth==4)){
    # second <- sapply(toBeConverted[which(lngth==4)],
    # function(x)  paste(strsplit(x,'')[[1]][2:3],collapse=''))
    sec2trd <- stringr::str_sub(toBeConverted[lngth==4], start = 2, end = 3)
    for(sec in unique(sec2trd)){
      tbc <- toBeConverted[lngth==4][
        grep(sprintf('^%s',paste(str,sec,sep='')),
             toBeConverted[lngth==4])
        ]
      fourth <- stringr::str_sub(tbc, start = 4, end = 4)  
      if(add.periods) {
        outStr <- sprintf("%sOR REGEXP_LIKE (%s,'^%s%s.[%s]','im')\n",
                          outStr,colName,str,sec,
                          paste(fourth,collapse=','))
      }
      else {
        outStr <- sprintf("%sOR REGEXP_LIKE (%s,'^%s%s[%s]','im')\n",
                          outStr,colName,str,sec,
                          paste(fourth,collapse=','))
      }
    }
  }
  
  return(outStr)
}
