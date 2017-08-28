#' Convert between ICD9 codes and ICD10 codes
#' 
#' @param code A vector of ICD codes to convert.
#' @param input Choose ICD version, can be "ICD9" or "ICD10".
#' @param default Logical value. If TRUE, then the default mapping is used. Otherwise use the 
#' possible mappings option which would return more results. 
#' @return A character vector of conversion results. 
#' @details The function utilizes HIPAASpace api to convert diagnosis codes between ICD9 and ICD10. Currently 
#' the function uses the mapcode option of the api. 
#' @export

# require packages httr and jsonlite
getICD_api = function(code, input = c("ICD9", "ICD10"), default = TRUE) {
  mapping = ifelse(default, "DefaultMapping", "PossibleMappings")
  In = match.arg(input, c("ICD9", "ICD10"))
  Out = setdiff(c("ICD9", "ICD10"), In)
  direction = ifelse(In == "ICD9", "icd9to10", "icd10to9")
  Direction = ifelse(In == "ICD9", "ICD9ToICD10", "ICD10ToICD9")
  output = unlist(lapply(code, function(c) {
    uri = sprintf("http://www.HIPAASpace.com/api/%s/mapcode?codeType=dx&q=%s&rt=json&token=0D9FA6344B504920BB44376902FB369427C56ED0023A4FFEBA130B41BF8E1BA0", 
                  direction, c)
    json_content = content(GET(uri), as = "text", encoding = "UTF-8") %>% fromJSON
    json_content[[Direction]][[mapping]][[Out]]$`@code`
  }))
  return(unique(output))
}

getICD = function(ICD9vec) {
  data(ICD9to10_api)
  output = unique(unlist(lapply(ICD9vec, function(code) ICD9to10_api[[code]])))
  return(output)
}
