#' Read in a TOML file and output a codelist for \code{writeSQL} function
#' 
#' @param file A file path to the TOML file. 
#' @param minGroup The minimum number of occurrences for a starting string pattern
#' to be put in the \code{$LIKE} list. 
#' @param maxLength The maximum length of starting strings considered in \code{$LIKE}. 
#' @param input,output Specify the version of ICD codes in the input or output. Choose 
#' between 'ICD9' and 'ICD10'. 
#' @param option Choose from 1 to 3 to specify what table to be used in conversion. 
#' Choose 1 for ICD9to10, 2 for ICD10to9, and 3 for using both and returning the union of
#' two sets of results. Default is 1.
#' @return A list object with three elements. 
#' \item{num}{code list of the numerator}
#' \item{elig}{code list of the inclusion}
#' \item{excl}{code list of the exclusion}
#' Each element is a code list that has two elements, \code{LIKE} and \code{IN}. And 
#' they can be fed into function \code{writeSQL} as the \code{codeList} parameter. 
#' @export
toml2code <- function(file, minGroup = 2, maxLength = 4, input = c('ICD9', 'ICD10'), 
                      output = c('ICD9', 'ICD10'), option = c("1", "2", "3")) {
  data("ICD9to10", "ICD10to9")
  toml <- parseTOML(file)
  In <- match.arg(input, c('ICD9', 'ICD10'))
  Out <- match.arg(output, c('ICD9', 'ICD10'))
  op <- match.arg(as.character(option), c("1", "2", "3"))

  num <- toml$numerator
  elig <- toml$denominator$eligible
  excl <- toml$denominator$exclude
 
  if("CPT" %in% names(num)) num <- removex(unlist(num$CPT))
  else {
    temp <- lapply(num, function(x){
      if("CPT" %in% names(x)) removex(x$CPT)
    })
    num <- unlist(temp)
  }

  if(In %in% names(elig)) elig <- removex(unlist(elig[[In]]))
  else {
    temp <- lapply(elig, function(x){
      if(In %in% names(x)) removex(x[[In]])
    })
    elig <- unlist(temp)
  } 

  if(In %in% names(excl)) excl <- removex(unlist(excl[[In]]))
  else {
    temp <- lapply(excl, function(x){
      if(In %in% names(x)) removex(x[[In]])
    })
    excl <- unlist(temp)
  }

  data("dict_CPT")
  num_list <- convertToLike(num, dict_CPT)
  
  if(In == 'ICD9') {
    elig <- unique(unlist(lapply(elig, function(str) {
      ICD9to10$ICD9[grep(sprintf("^%s", str), ICD9to10$ICD9)]
    })))
    excl <- unique(unlist(lapply(excl, function(str) {
      ICD9to10$ICD9[grep(sprintf("^%s", str), ICD9to10$ICD9)]
    })))
  }
  if(In == 'ICD10') {
    elig <- unique(unlist(lapply(elig, function(str) {
      ICD10to9$ICD10[grep(sprintf("^%s", str), ICD10to9$ICD10)]
    })))
    excl <- unique(unlist(lapply(excl, function(str) {
      ICD10to9$ICD10[grep(sprintf("^%s", str), ICD10to9$ICD10)]
    })))
  }
  
  if(Out == 'ICD10') {
    if(In == 'ICD9') {elig <- getICD10(elig, option = op); excl <- getICD10(excl, option = op)}
    elig_list <- convertToLike(elig, ICD9to10$ICD10, minGroup, maxLength)
    excl_list <- convertToLike(excl, ICD9to10$ICD10, minGroup, maxLength)
  }
  if(Out == 'ICD9') {
    if(In == 'ICD10') {elig <- getICD9(elig, option = op); excl <- getICD9(excl, option = op)}
    elig_list <- convertToLike(elig, ICD10to9$ICD9, minGroup, maxLength)
    excl_list <- convertToLike(excl, ICD10to9$ICD9, minGroup, maxLength)
  }
  
  return(list(num = num_list, elig = elig_list, excl = excl_list))
}

#' @export
toml2num <- function(file){
  toml <- parseTOML(file)
  num <- toml$numerator
  if("CPT" %in% names(num)) num <- removex(unlist(num$CPT))
  else {
    temp <- lapply(num, function(x){
      if("CPT" %in% names(x)) removex(x$CPT)
    })
    num <- unlist(temp)
  }
  
  data("dict_CPT")
  num_list <- convertToLike(num, dict_CPT)
  return(list(num=num_list))
}