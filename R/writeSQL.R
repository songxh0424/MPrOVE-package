#' Create SQL commands 
#' @description This function creates a series of "REGEXP_LIKE" and "IN" SQL 
#' commands to query records with certain ICD codes. 
#' @param codelist List output from \code{createLIKE} or \code{convertToLike}. 
#' @param colName The corresponding column name to put in "REGEXP_LIKE" commands.
#' @param file The path to output txt file. 
#' @param write.file A logical value controlling whether or not to write the output
#' into \code{file}. Default is \code{TRUE}. When \code{write.file} is \code{FALSE}, 
#' the function will return the output as a character string instead. 
#' @details This function calls \code{convert2regex} to create "REGEXP_LIKE" 
#' commands to query codes in \code{codelist$LIKE}, and creates "IN" commands 
#' to query codes in \code{codelist$IN}
#' @return This function doesn't return any value. It creates a txt file to the 
#' specified path. The txt file contains a series of "REGEXP_LIKE" and "IN" commands
#' connected by "OR". 
#' @seealso \code{\link{convert2regex}}, \code{\link{createLIKE}}, 
#' \code{\link{convertToLike}}
#' @examples 
#' bpExclICD9 <- c(1400:2089,2300:2399,8000:8399,8500:8549,8600:8699,
#'                 9050:9099,92611,92612,929,9520:9529,9580:9599,3040:3042,
#'                 3044,3054:3057,34460,7292,4210,4211,4219,0380:0389,01,
#'                 7300:7309,7832,78079,7808,2859)
#' bpExclICD10 <- getICD10(bpExclICD9)
#' codeList <- convertToLike(codes = bpExclICD10, dict = ICD9to10$ICD10,2,4)
#' 
#' writeSQL(codeList,'DX_CD',file='BackPain-ICD10-exclusions.txt')
#' @export
writeSQL <- function(codeList,colName,file,write.file=TRUE){
  nm <- names(codeList$LIKE)
  # lngth <- sapply(nm,function(x) length(strsplit(x,'')[[1]]))
  lngth = stringr::str_length(nm)
  
  outstr = ' '
  if(any(lngth==1)){
    for(str in nm[which(lngth==1)]){
      outstr <- sprintf("%sOR %s LIKE '%s%%'\n",outstr,colName,str)
    }
  }
  
  if(any(lngth>1)){
    # one <- sapply(nm[which(lngth > 1)],function(x) strsplit(x,'')[[1]][1])
    one = stringr::str_sub(nm[lngth > 1], end = 1)
    for(o in unique(one)){
      outstr <- sprintf("%s%s\n", outstr, convert2regex(nm,o,colName))
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
      outstr <- sprintf("%sOR %s IN (%s)\n",outstr,colName,
                        paste(sprintf("'%s'",IN.o),collapse=','))
    }
    
  }
  if(write.file) cat(outstr, file = file)
  else return(outstr)
}