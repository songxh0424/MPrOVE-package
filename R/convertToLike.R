#' Create lists of ICD10 codes to be used in generating SQL queries
#' @description \code{createLIKE} and \code{convertToLike} create two lists of ICD10 codes. The first one contains 
#' a sets of common patterns of the first few digits of the ICD10 codes, which can 
#' then be used in "LIKE" SQL commands. The second list contains ICD10 codes that 
#' don't share common patterns, which can be put in "IN" SQL commands. 
#' @param codes A vector of ICD10 codes as strings. Exclusions. 
#' @param dict A vector of ICD10 codes as strings.
#' @param n The length of starting strings used for grouping ICD10 codes. 
#' @param minGroup The minimum number of occurrences for a starting string pattern
#' to be put in the \code{$LIKE} list. 
#' @param maxLength The maximum length of starting strings considered in \code{$LIKE}. 
#' @details \code{convertToLike} performs similar task to \code{createLIKE}, except 
#' that instead of using a fixed length, it considers all starting strings with length 
#' from 1 to \code{maxLength}. 
#' 
#' Note that the ICD10 codes in the output are all from \code{codes}. 
#' @return Both functions return a list object.
#' \item{LIKE}{Sub-lists of starting string patterns.}
#' \item{IN}{All other ICD10 codes in \code{codes} that don't match 
#' those patterns. }
#' 
#' @examples 
#' ICD9to10.dat = as.data.frame(ICD9to10, stringsAsFactors = FALSE)
#' # Exclusions
#' bpExclICD9 <- c(1400:20899,23000:23999,8000:83999,85000:85499,86000:86999,
#'                 90500:90999,92611,92612,929,95200:95299,95800:95999,30400:30429,
#'                 30440:30449,30540:30579,34460,72920:72929,4210,4211,4219,
#'                 03800:03899,73000:73099,78320:78329,78079,78080:78089,28590:28599,
#'                 72142,72191,72270,72273,7244)
#' bpExclICD10 <- unique(getICD10(bpExclICD9)) 
#' 
#' createLIKE(codes = bpExclICD10, dict = ICD9to10.dat$ICD10)
#' 
#' convertToLike(codes = bpExclICD10, dict = ICD9to10.dat$ICD10, maxLength = 4)
createLIKE <- function(codes,dict,n=2,minGroup=2){
  # the naming convention assumes n=2, but the function should be generic
  
  # get all n letter combinations starting strings in codes
  # two <- sapply(strsplit(codes,''),function(x) paste(x[1:n],collapse=''))
  two = sapply(codes, function(str) str_sub(str, end = n))
  twoTab <- table(two)
  two <- names(twoTab[which(twoTab >= minGroup)])
  
  # flag n letter codes for which subsets exhaust dict
  twoFlag <- sapply(two,checkLike,codes=codes,dict=dict)
  codesList <- lapply(two[which(twoFlag)],function(str){
    # codes[grep(sprintf('^%s',str),codes)]
    codes[str_sub(codes, end = n) == str]
  })
  names(codesList) <- two[which(twoFlag)]
  
  remainder <- setdiff(codes,unlist(codesList))
  return(list('LIKE'=codesList,'IN'=remainder))
}
#' @rdname createLIKE
convertToLike <- function(codes,dict,minGroup=2,maxLength=3){
  # given a vector of codes, collect similar codes into like statements
  # must not return any srings in dict but not in codes #
  # minGroup is a control parameter for how many codes are needed to convert 
  # to a 'LIKE' statement
  # maxLength is the largest starting string to consider grouping
  
  out <- createLIKE(codes,dict,1,minGroup)
  if(maxLength > 1 & length(out$IN)>=minGroup){
    for(n in 2:maxLength){
      tmp <- createLIKE(out$IN,dict,n,minGroup)
      out$LIKE <- c(out$LIKE,tmp$LIKE)
      out$IN <- tmp$IN
      if(length(tmp$IN)<minGroup) break
    }
  }
  return(out)
}