#' Mappings from ICD-9 codes to ICD-10 codes
#' @description \code{ICD9to10} is a data frame read from 2017_I9gem.txt file. It 
#' maps ICD-9 codes to ICD-10 codes. 
#' @usage data(ICD9to10)
#' @format A data frame with 24690 rows and 3 columns: 
#' \itemize{
#'   \item \code{ICD9}:   Character. ICD-9 codes.
#'   \item \code{ICD10}:   Character. ICD-10 codes. 
#'   \item \code{Flags}:   Character. Special mapping flags. Each has 5 digits. The 5 digits
#'   represent, from left to right, "Approximate", "No map", "Combination", 
#'   "Scenario" and "Choice List". 
#' }
#' @details For further information about the mapping system, see 
#' Dxgem_guide_2017.pdf and GemsTechDoc_2017.pdf. 
#' @source \link{https://www.cms.gov/Medicare/Coding/ICD10/2017-ICD-10-CM-and-GEMs.html}
#' @export
"ICD9to10"