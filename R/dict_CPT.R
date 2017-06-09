#' Complete range of CPT codes
#' @name dict_CPT
#' @docType data
#' @description \code{dict_CPT} is a vector containing all CPT codes. 
#' @usage data(dict_CPT)
#' @format A character vector of 82206 elements. 
#' @details There are three types of CPT code: Category I, Category II, and Category III.
#' \code{dict_CPT[1:80623] are from Category I, which are all numerical. } 
#' \code{dict_CPT[80624:82014]} are from Category II and all of them are numerical codes 
#' with an "F" attached at the end. \code{dict_CPT[82015:82206]} are from Category III. 
#' They are similar to Category II codes except the ending character is "T". Regardless 
#' of category, all CPT codes have five characters. 
#' @source \link{https://en.wikipedia.org/wiki/Current_Procedural_Terminology#cite_note-1}
NULL