#' text_clean.whitespace2
#'
#' check nominal if they are nominal
#' @param Click here and there
#' @export
#' @examples
#' checking.nominal()
#' @importFrom magrittr %>%
#'

text_clean.whitespace2<-function(vec) {
  rt<-sapply(vec,function(strng) {
    while(gregexpr("  ",strng)[[1]][1]!=-1) strng <- strng %>% gsub("  "," ",.)
    k<-0
    while(substr(strng,k+1,k+1)==" ") k<-k+1
    strng<-substr(strng,k+1,nchar(strng))
    k<-nchar(strng)
    while(substr(strng,k,k)==" ") k<-k-1
    strng<-substr(strng,1,k)
    return(strng)
  }) %>% unlist
  return(rt)
}

