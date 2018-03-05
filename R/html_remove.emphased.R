#' html_remove.emphased
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
html_remove.emphased<-function(x) {
  emph<-c("i","b","em","sub","sup")
  emph<-sapply(emph,function(x) c(paste0("<",x,">"),paste0("</",x,">"))) %>% c
  f<-sapply(emph,function(e1) x<<-x%>%gsub(e1,"",.))
  return(x)
}

