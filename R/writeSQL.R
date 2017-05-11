#' Create SQL commands 
#' @description This function creates a series of "REGEXP_LIKE" and "IN" SQL 
#' commands to query records with certain ICD codes. 
#' @param codelist List output from \code{createLIKE} or \code{convertToLike}. 
#' @param colName The corresponding column name to put in "REGEXP_LIKE" commands.
#' @param file The path to output txt file. 
#' @details This function calls \code{convert2regex} to create "REGEXP_LIKE" 
#' commands to query codes in \code{codelist$LIKE}, and creates "IN" commands 
#' to query codes in \code{codelist$IN}
#' @return This function doesn't return any value. It creates a txt file to the 
#' specified path. The txt file contains a series of "REGEXP_LIKE" and "IN" commands
#' connected by "OR". 
#' @seealso \code{\link{convert2regex}}, \code{\link{createLIKE}}, 
#' \code{\link{convertToLike}}
#' @examples 
#' bpExclICD9 <- c(1400:20899,23000:23999,8000:83999,85000:85499,86000:86999,
#'                 90500:90999,92611,92612,929,95200:95299,95800:95999,30400:30429,
#'                 30440:30449,30540:30579,34460,72920:72929,4210,4211,4219,
#'                 03800:03899,73000:73099,78320:78329,78079,78080:78089,28590:28599,
#'                 72142,72191,72270,72273,7244)
#' bpExclICD10 <- unique(getICD10(bpExclICD9)) 
#' codeList <- convertToLike(codes = bpExclICD10, dict = ICD9to10$ICD10,2,4)
#' 
#' writeSQL(codeList,'DX_CD',file='BackPain-ICD10-exclusions.txt')
writeSQL <- function(codeList,colName,file){
  sink(file)
  nm <- names(codeList$LIKE)
  # lngth <- sapply(nm,function(x) length(strsplit(x,'')[[1]]))
  lngth = stringr::str_length(nm)
  
  if(any(lngth==1)){
    for(str in nm[which(lngth==1)]){
      cat(sprintf("OR %s LIKE '%s%%'\n",colName,str))
    }
  }
  
  if(any(lngth>1)){
    # one <- sapply(nm[which(lngth > 1)],function(x) strsplit(x,'')[[1]][1])
    one = stringr::str_sub(nm[lngth > 1], end = 1)
    for(o in unique(one)){
      cat(convert2regex(nm,o,colName),'\n')
    }
  }
  
  if(length(codeList$IN)>0){
    IN <- codeList$IN[order(codeList$IN)]
    IN <- IN[which(IN != 'NoDx')]
    # one <- sapply(IN,function(x) strsplit(x,'')[[1]][1])
    one = stringr::str_sub(IN, end = 1)
    for(o in unique(one)){
      IN.o <- IN[grep(sprintf('^%s',o),IN)]
      # IN.o <- sapply(IN.o,addPeriods)
      IN.o <- addPeriods(IN.o)
      cat(sprintf("OR %s IN (%s)\n",colName,paste(sprintf("'%s'",IN.o),collapse=',')))
    }
    
  }
  
  sink()
}