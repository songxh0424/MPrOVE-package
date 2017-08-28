#' Create SQL commands 
#' @description This function creates a series of "REGEXP_LIKE" and "IN" SQL 
#' commands to query records with certain ICD codes. 
#' @param codelist List output from \code{createLIKE} or \code{convertToLike}. 
#' @param colName The corresponding column name to put in "REGEXP_LIKE" commands.
#' @param file The path to output txt file. 
#' @param write.file A logical value controlling whether or not to write the output
#' into \code{file}. Default is \code{TRUE}. When \code{write.file} is \code{FALSE}, 
#' the function will return the output as a character string instead. 
#' @param add.periods If TRUE, processes each 'IN' statement with \link{\code{addPeriods}}. Defaults to FALSE.
#' @details This function calls \code{convert2regex} to create "REGEXP_LIKE" 
#' commands to query codes in \code{codelist$LIKE}, and creates "IN" commands 
#' to query codes in \code{codelist$IN}
#' @return If write.file=FALSE (default), returns a partial sql query as a string.
#' Otherwise, writes that string to a text file as specified.
#' The returned string/file contains a series of "REGEXP_LIKE" and "IN" commands
#' connected by "OR". 
#' @seealso \code{\link{convert2regex}}, \code{\link{createLIKE}}, 
#' \code{\link{convertToLike}}
#' @export
#' @examples 
#' ICD9 <- c(1400:2089,2300:2399,8000:8399,8500:8549,8600:8699,
#'                 9050:9099,92611,92612,929,9520:9529,9580:9599,3040:3042,
#'                 3044,3054:3057,34460,7292,4210,4211,4219,0380:0389,01,
#'                 7300:7309,7832,78079,7808,2859)
#' ICD10 <- getICD10(ICD9)
#' codeList <- convertToLike(codes = ICD10, dict = ICD9to10$ICD10,2,4)
#' 
#' cat(writeSQL(codeList,'DX_CD',file=NULL,write.file=FALSE,add.periods=FALSE))
#' @export
writeSQL <- function(codeList,colName,file=NULL,write.file=TRUE,add.periods=FALSE){
  if(is.null(codeList)) {
    warning("Empty codeList!")
    return(NULL)
  }
  
  nm <- names(codeList$LIKE)
  lngth = stringr::str_length(nm)
  
  outstr = ''
  if(any(lngth==1)){
    for(str in nm[which(lngth==1)]){
      outstr <- sprintf("%sOR %s LIKE '%s%%'\n",outstr,colName,str)
    }
  }
  
  if(any(lngth>1)){
    one = stringr::str_sub(nm[lngth > 1], end = 1)
    for(o in unique(one)){
      outstr <- sprintf("%s%s", outstr, convert2regex(nm,o,colName,add.periods))
    }
  }
  
  if(length(codeList$IN)>0){
    IN <- codeList$IN[order(codeList$IN)]
    IN <- IN[which(IN != 'NoDx')]
    one = stringr::str_sub(IN, end = 1)
    for(o in unique(one)){
      IN.o <- IN[grep(sprintf('^%s',o),IN)]
      if(add.periods) IN.o <- addPeriods(IN.o)
      strInPar <- sprintf("'%s'",IN.o)
      leng <- length(IN.o)
      if(leng > 100) {
        id <- c(seq_along(IN.o), 100 * (1:floor(leng/100)) + 0.5)
        strInPar <- append(strInPar, rep(';', floor(leng/100)))
        strInPar <- strInPar[order(id)]
      }
      strInPar <- str_replace_all(paste(strInPar,collapse=','),',;,',';')
      strInPar <- str_split(strInPar, ';')[[1]]
      strIN <- sprintf("OR %s IN (%s)\n",colName,strInPar)
      outstr <- paste0(c(outstr, strIN), collapse = "")
    }
    
  }
  # replace first 
  outstr <- str_replace(outstr,'OR ','')

  if(write.file) cat(outstr, file = file)
  else return(outstr)
}
