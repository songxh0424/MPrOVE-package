#' Create "REGEXP_LIKE" SQL commands
#' @description This function creates a series of "REGEXP_LIKE" SQL commands, given 
#' a set of starting strings of ICD codes and a starting pattern to match. 
#' @param nm A character vector of starting strings of ICD10 codes. 
#' @param str A specific starting string that is used to match the ones in \code{nm}.
#' Only the matching ones in \code{nm} are considered to create the output SQL 
#' commands. 
#' @param colName The corresponding column name to put in "REGEXP_LIKE" commands.
#' @details \code{str} is usually a very short string. The function first extracts 
#' all the strings in \code{nm} that start with \code{str}. Then those strings are
#' divided into groups that share the first 1, 2, or 3 letters, with only the last
#' letter being different. For each of these groups, a SQL command like this is 
#' created, "OR REGEXP_LIKE (DX_CD,'^I0[1,2,5,6,7,8]','im')".  
#' @return A character string of a series of "REGEXP_LIKE" SQL commands. 
#' @seealso \code{\link{writeSQL}}
#' @examples 
#' bpExclICD9 <- c(1400:20899,23000:23999,8000:83999,85000:85499,86000:86999,
#'                 90500:90999,92611,92612,929,95200:95299,95800:95999,30400:30429,
#'                 30440:30449,30540:30579,34460,72920:72929,4210,4211,4219,
#'                 03800:03899,73000:73099,78320:78329,78079,78080:78089,28590:28599,
#'                 72142,72191,72270,72273,7244)
#' bpExclICD10 <- unique(getICD10(bpExclICD9)) 
#' codeList <- convertToLike(codes = bpExclICD10, dict = ICD9to10$ICD10,2,4)
#' nm <- names(codeList$LIKE)
#' 
#' cat(convert2regex(nm,'C',colName = 'DX_CD'))
convert2regex <- function(nm,str,colName){
  toBeConverted <- nm[grep(sprintf('^%s',str),nm)]
  # lngth <- sapply(toBeConverted,function(x) length(strsplit(x,'')[[1]]))
  lngth <- stringr::length(toBeConverted)
  
  outStr <- ' ' #iniatilize
  if(any(lngth==2)){
    # second <- sapply(toBeConverted[which(lngth==2)],function(x) strsplit(x,'')[[1]][2])
    second <- stringr::sub(toBeConverted[lngth==2], start = 2, end = 2)
    outStr <- sprintf("%sOR REGEXP_LIKE (%s,'^%s[%s]','im')\n",outStr,colName,str,
                      paste(second,collapse=','))
  }
  
  if(any(lngth==3)){
    # second <- sapply(toBeConverted[which(lngth==3)],function(x) strsplit(x,'')[[1]][2])
    second <- stringr::sub(toBeConverted[lngth==3], start = 2, end = 2)
    for(sec in unique(second)){
      tbc <- toBeConverted[lngth==3][
        grep(sprintf('^%s',paste(str,sec,sep='')),
             toBeConverted[lngth==3])
        ]
      third <- stringr::sub(tbc, start = 3, end = 3) 
      outStr <- sprintf("%s OR REGEXP_LIKE (%s,'^%s%s[%s]','im')\n",
                        outStr,colName,str,sec,
                        paste(third,collapse=','))
    }
  }
  
  if(any(lngth==4)){
    # second <- sapply(toBeConverted[which(lngth==4)],
    # function(x)  paste(strsplit(x,'')[[1]][2:3],collapse=''))
    sec2trd <- stringr::sub(toBeConverted[lngth==4], start = 2, end = 3)
    for(sec in unique(sec2trd)){
      tbc <- toBeConverted[lngth==4][
        grep(sprintf('^%s',paste(str,sec,sep='')),
             toBeConverted[lngth==4])
        ]
      fourth <- stringr::sub(tbc, start = 4, end = 4)  
      outStr <- sprintf("%s OR REGEXP_LIKE (%s,'^%s%s.[%s]','im')\n",
                        outStr,colName,str,sec,
                        paste(fourth,collapse=','))
    }
  }
  
  return(outStr)
}