% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ICD10to9.R
\docType{data}
\name{ICD10to9}
\alias{ICD10to9}
\title{Mappings from ICD-10 codes to ICD-9 codes}
\format{A data frame with 81298 rows and 3 columns: 
\itemize{
  \item \code{ICD10}:   Character. ICD-10 codes.
  \item \code{ICD9}:   Character. ICD-9 codes. 
  \item \code{Flags}:   Character. Special mapping flags. Each has 5 digits. The 5 digits
  represent, from left to right, "Approximate", "No map", "Combination", 
  "Scenario" and "Choice List". 
}}
\source{
\link{https://www.cms.gov/Medicare/Coding/ICD10/2017-ICD-10-CM-and-GEMs.html}
}
\usage{
data(ICD10to9)
}
\description{
\code{ICD10to9} is a data frame read from 2017_I10gem.txt file. It 
maps ICD-10 codes to ICD-9 codes.
}
\details{
For further information about the mapping system, see 
Dxgem_guide_2017.pdf and GemsTechDoc_2017.pdf.
}
