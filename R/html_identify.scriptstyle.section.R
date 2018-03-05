#' html_identify.scriptstyle.section
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

html_identify.scriptstyle.section<-function(x) {
  delete.rows<-rbind(data.frame(x1=grep("<script",x),x2=grep("</script",x)),
                     data.frame(x1=grep("<style",x),x2=grep("</style",x)))
  dr<-apply(delete.rows,1,function(y) c(y[1]:y[2])) %>% unlist
  return(x[!1:length(x)%in%dr])
}
