#' text_clean.whitespace
#'
#' check nominal if they are nominal
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples 
#' checking.nominal()
#' @importFrom magrittr %>%
#' 

text_clean.whitespace<-function(vec) {
  rt<-lapply(vec,function(strng) {
    while(gregexpr("  ",strng)[[1]][1]!=-1) strng <- strng %>% gsub("  "," ",.)
    if (substr(strng,1,1)==" ") strng<-substr(strng,2,nchar(strng))
    if (substr(strng,nchar(strng),nchar(strng))==" ") strng<-substr(strng,1,nchar(strng)-1)
    return(strng)
  }) %>% unlist
  return(rt)
}