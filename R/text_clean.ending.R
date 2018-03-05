#' text_clean.ending
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

text_clean.ending<-function(strng) {
  if (substr(strng,nchar(strng),nchar(strng))%in%c(":",";","/","\\")) substr(strng,1,nchar(strng)-1)
}