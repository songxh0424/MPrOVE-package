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
#' @param thres A percentage threshold for passing the check. 
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
#' # Exclusions
#' bpExclICD9 <- c(1400:2089,2300:2399,8000:8399,8500:8549,8600:8699,
#'                 9050:9099,92611,92612,929,9520:9529,9580:9599,3040:3042,
#'                 3044,3054:3057,34460,7292,4210,4211,4219,0380:0389,01,
#'                 7300:7309,7832,78079,7808,2859)
#' bpExclICD10 <- getICD10(bpExclICD9)
#' 
#' createLIKE(codes = bpExclICD10, dict = ICD9to10$ICD10)
#' 
#' convertToLike(codes = bpExclICD10, dict = ICD9to10$ICD10, maxLength = 4)
#' @export
createLIKE <- function(codes,dict,n=2,minGroup=2,thres=100){
  # the naming convention assumes n=2, but the function should be generic
  
  if(length(codes) == 0) return(NULL)
  
  # get all n letter combinations starting strings in codes
  # two <- sapply(strsplit(codes,''),function(x) paste(x[1:n],collapse=''))
  two = sapply(codes, function(str) str_sub(str, end = n))
  twoTab <- table(two)
  two <- names(twoTab[which(twoTab >= minGroup)])
  # if two is an empty vector, put all codes in 'IN' list directly
  if(length(two) == 0) return(list('LIKE' = list(), 'IN' = codes))
  
  # flag n letter codes for which subsets exhaust dict
  twoFlag <- sapply(two,checkLike,codes=codes,dict=dict,thres=thres)
  codesList <- lapply(two[which(twoFlag)],function(str){
    # codes[grep(sprintf('^%s',str),codes)]
    codes[str_sub(codes, end = n) == str]
  })
  names(codesList) <- two[which(twoFlag)]
  
  remainder <- setdiff(codes,unlist(codesList))
  return(list('LIKE'=codesList,'IN'=remainder))
}
#' @rdname createLIKE
#' @export
convertToLike <- function(codes,dict,minGroup=2,maxLength=3,thres=100){
  # given a vector of codes, collect similar codes into like statements
  # must not return any srings in dict but not in codes #
  # minGroup is a control parameter for how many codes are needed to convert 
  # to a 'LIKE' statement
  # maxLength is the largest starting string to consider grouping
  
  out <- createLIKE(codes,dict,1,minGroup,thres=thres)
  if(maxLength > 1 & length(out$IN)>=minGroup){
    for(n in 2:maxLength){
      tmp <- createLIKE(out$IN,dict,n,minGroup,thres=thres)
      out$LIKE <- c(out$LIKE,tmp$LIKE)
      out$IN <- tmp$IN
      if(length(tmp$IN)<minGroup) break
    }
  }
  return(out)
}
