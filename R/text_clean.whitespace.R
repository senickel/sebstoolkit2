#' text_clean.whitespace
#'
#' Cleans double whitespaces and whitespace at the beginning or end of string.
#' @param vec character string or vector of character strings
#' @examples
#' test<-c("te    st"," test ")
#' test
#' text_clean.whitespace(test)
#' @importFrom magrittr %>%
#'

text_clean.whitespace<-function(vec) {
  rt<-lapply(vec,function(strng) {
    if (is.na(strng)) return(strng)
    while(gregexpr("  ",strng)[[1]][1]!=-1) strng <- strng %>% gsub("  "," ",.)
    if (substr(strng,1,1)==" ") strng<-substr(strng,2,nchar(strng))
    if (substr(strng,nchar(strng),nchar(strng))==" ") strng<-substr(strng,1,nchar(strng)-1)
    return(strng)
  }) %>% unlist
  return(rt)
}
