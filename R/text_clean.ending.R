#' text_clean.ending
#'
#'
#' @param strng It's a string

#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

text_clean.ending<-function(strng) {
  if (substr(strng,nchar(strng),nchar(strng))%in%c(":",";","/","\\")) substr(strng,1,nchar(strng)-1)
}
