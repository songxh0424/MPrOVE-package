#' Mappings from ICD-10 codes to ICD-9 codes
#' @description \code{ICD10to9} is a data frame read from 2017_I10gem.txt file. It 
#' maps ICD-10 codes to ICD-9 codes. 
#' @usage data(ICD10to9)
#' @format A data frame with 81298 rows and 3 columns: 
#' \itemize{
#'   \item \code{ICD10}:   Character. ICD-10 codes.
#'   \item \code{ICD9}:   Character. ICD-9 codes. 
#'   \item \code{Flags}:   Character. Special mapping flags. Each has 5 digits. The 5 digits
#'   represent, from left to right, "Approximate", "No map", "Combination", 
#'   "Scenario" and "Choice List". 
#' }
#' @details For further information about the mapping system, see 
#' Dxgem_guide_2017.pdf and GemsTechDoc_2017.pdf. 
#' @source \link{https://www.cms.gov/Medicare/Coding/ICD10/2017-ICD-10-CM-and-GEMs.html}
#' @export
"ICD10to9"